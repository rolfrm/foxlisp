#include <iron/full.h>
#include "foxlisp.h"


typedef enum{
	     DITHER_NONE,
	     DITHER_FLOYD_STEINBERG
}DITHERING;

extern DITHERING dithering;

typedef struct{
  // expensive 32-bit color channels. oh well.
  f32 r, g, b;
}rgb;

typedef struct{
  f32 h, s, v;
}hsv;


hsv rgb2hsv(rgb color);
rgb hsv2rgb(hsv color);

rgb rgb_blend(rgb a, rgb b, f32 ratio);
hsv hsv_blend(hsv a, hsv b, f32 ratio);

rgb rgb_add(rgb a, rgb b);

#define RGB(r2,g2,b2) {.r = r2, .g = g2, .b = b2}
#define dot(x,y) vec2_dot(x,y)
#define dot2(x) vec2_dot(x,x)

#define VEC2(x2,y2) {.x = x2, .y = y2}
#define VEC3(x3,y3, z3) {.x = x3, .y = y3, .z = z3}

rgb get_color(vec2 p);
rgb df_mix(rgb color, rgb bg, f32 dist);
extern f32 pixel_size;

f32 noise(vec2 coords);
f32 fract(f32 x);
rgb df_gradient(vec2 p, rgb color1, rgb color2, vec2 start, vec2 stop);
vec2 df_repeat(vec2 p, f32 mod);
f32 df_mod(f32 vec, f32 mod);
f32 df_square(vec2 p, vec2 center, vec2 radius);
f32 df_round_square(vec2 p, vec2 center, vec2 radius, f32 corner_radius);
f32 df_circle(vec2 p, vec2 center, f32 radius);
f32 df_outline(f32 dist, f32 width);
f32 polygon_distance(vec2 p, vec2 * v, u32 len);
// pseudo coloring for debugging distance fields.
rgb df_pseudo(f32 x);

#define MIN3(a,b,c) MIN(MIN(a,b),c)
#include "distance_fields.c"

vec2 p1 = {.x = 0, .y = 3}; // top left coordinate.
vec2 p2 = {.x = 0, .y = 10}; // bottom right coordinate..
rgb color1 = {.r = 0.5, .g = 0.8, .b = 0.5}; // light blue
rgb color2 = {.r = 0.3, .g = 0.5, .b = 0.2}; // white

rgb get_color(vec2 p){
  // (P - P1) dot (P - P2)
  // Linearily interpolate the colors.
  f32 n = noise(p);
  vec2 lvec = vec2_sub(p2, p1);
  f32 ratio = vec2_dot(vec2_sub(p, p1), lvec) / vec2_dot(lvec, lvec);
  ratio = CLAMP(ratio,0,1.0);
  return rgb_blend(color1, color2, ratio + 1 * (n - 0.5f) / 128.0f);  
}



float box(vec3 p, vec3 b )
{
  vec3 q = vec3_sub(vec3_abs(p), b);
  
  return vec3_len(vec3_max(q, vec3_zero)) + MIN(MAX(q.x, MAX(q.y, q.z)), 0.0);
}

f32 clampf(f32 a, f32 b, f32 c){
  if(a < b) return b;
  if(a > c) return c;
  return a;
}

float vert_capsule( vec3 p, float h, float r )
{
  p.y -= clampf( p.y, 0.0, h );
  return vec3_len(p) - r;
}
f32 sphere(vec3 v, vec3 center, float radius){
  vec3 p = center;
  return vec3_len(vec3_sub(v, p)) - radius;
  
  
}
f32 d0(vec3 v, vec3 * c, vec3 center, float radius){
  *c = vec3_new(1, 0, 1);
  vec3 p = center;
  
  return vec3_len(vec3_sub(v, p)) - radius;
}


f32 d1(vec3 v, vec3 * c){
  *c = vec3_new(0, 0, 1);
  var d1 = d0(v, c, vec3_new(0,2,0), 1.0);
  var d2 = d0(v, c, vec3_new(-3,2,0), 0.5);
  var d3 = d0(v, c, vec3_new(3,3,0), 0.5);
  var d4 = box(vec3_sub(v,vec3_new(0,0.5,0.25)), vec3_new(0.5, 0.5, 0.5));
  
  var d =  MIN(d3, MIN(d1, MIN(d2, d4)));
  if(d == d1)
    *c = vec3_new(1, 0, 0);
  if(d == d2)
    *c = vec3_new(0, 1, 0);
  if(d == d3)
    *c = vec3_new(0, 0, 1);
  if(d == d4)
    *c = vec3_new(0, 1, 1);
  return d;
}

f32 tree(vec3 v, vec3 * c){
  v = vec3_scale(v, 1.2);
  //v.y += 2;
  f32 d1 = vert_capsule(vec3_sub(v, vec3_new(0,0,0)), 3.0, 0.5);

  f32 d2 = sphere(v, vec3_new(0, 4, 0), 1.5);
  f32 d3 = sphere(v, vec3_new(1.3, 3.9, -0.2), 1.6);
  f32 d4 = sphere(v, vec3_new(-0.3, 3.8, -1.0), 1.4);
  f32 d5 = sphere(v, vec3_new(0.8, 4.3, 1.2), 1.4);

  var dg = MIN(d2,d3);
  dg = MIN(d4, dg);
  dg = MIN(d5, dg);
  var d = MIN(d1, dg);
 
  
  if(d == d1)
    *c = vec3_new(0.8,0.4,0.4);
  if(d == dg){
    var rgb = get_color(vec2_new(v.x, v.y));;
    *c = vec3_new(rgb.r, rgb.g, rgb.b);
    //*c = vec3_new(0.5,0.8,0.5);
  }
  return d;
}



typedef struct{
  f32 (* sdf)(vec3 v, vec3 * color);
  void (* emit_point)(vec3 pt, vec3 color);
  f32 threshold;
}df_ctx;

vec3 sdf_gradient(df_ctx * ctx, vec3 pt, f32 size){
  vec3 c;
  var ptx = pt;
  var pty = pt;
  var ptz = pt;
  ptx.x += size * 0.2;
  pty.y += size * 0.2;
  ptz.z += size * 0.2;
  var dx1 = ctx->sdf(ptx, &c);
  var dy1 = ctx->sdf(pty, &c);
  var dz1 = ctx->sdf(ptz, &c);

  ptx.x -= size * 0.2 * 2;
  pty.y -= size * 0.2 * 2;
  ptz.z -= size * 0.2 * 2;
  var dx2 = ctx->sdf(ptx, &c);
  var dy2 = ctx->sdf(pty, &c);
  var dz2 = ctx->sdf(ptz, &c);

  var x = vec3_normalize(vec3_new(dx1 - dx2, dy1 - dy2, dz1 - dz2));
  return x;
}

vec3 trace_point(df_ctx * ctx, vec3 pt, f32 size){
  var x = sdf_gradient(ctx, pt, size);
  vec3 c;
  var d0 = ctx->sdf(pt, &c);

  var r = vec3_sub(pt, vec3_scale(x, d0));
  return r;
}

// idea: use the normal to reduce the number of planes.
void trace_point_cloud(df_ctx * ctx, vec3 position, f32 size){
  f32 d;
  vec3 c;
  d = ctx->sdf(position, &c);
  if(fabs(d) < size * 1.5 ){
    f32 s2 = size * 0.5;
    
    if(size < ctx->threshold * 1.5){
      size = size * 0.6;
      position = trace_point(ctx, position, size * 0.1);
      var g = sdf_gradient(ctx, position, size * 0.1);
      
      var s1 = vec3_normalize(vec3_mul_cross(g, vec3_new(g.y,g.z,g.x)));
      var s2 = vec3_mul_cross(g, s1);
      s1 = vec3_scale(s1, size);
      s2 = vec3_scale(s2, size);
      var pos2 = position;
      for(f32 j = -1; j < 2; j++){
      for(f32 i = -1; i < 2; i++){
        position = vec3_add(vec3_add(vec3_scale(s2, j * 2 ), vec3_scale(s1, i * 2)), pos2);
        position = trace_point(ctx, position, size * 0.1);
        
        vec3 p1 = vec3_add(position, vec3_add(s1, s2));
        vec3 p2 = vec3_add(position, vec3_sub(s1, s2));
        vec3 p3 = vec3_sub(position, vec3_add(s1, s2));
        vec3 p4 = vec3_sub(position, vec3_sub(s1, s2));
        p1 = trace_point(ctx, p1, size * 0.1);
        p2 = trace_point(ctx, p2, size * 0.1);
        p3 = trace_point(ctx, p3, size * 0.1);
        p4 = trace_point(ctx, p4, size * 0.1);

        ctx->sdf(p1, &c);
        
        ctx->emit_point(p1, c);
        ctx->sdf(p2, &c);
        
        ctx->emit_point(p2, c);
        ctx->sdf(p4, &c);
        ctx->emit_point(p4, c);
        ctx->sdf(p2, &c);
        
        ctx->emit_point(p2, c);
        ctx->sdf(p3, &c);
        ctx->emit_point(p3, c);
        ctx->sdf(p4, &c);
        ctx->emit_point(p4, c);
      }
      }
      
    }
    else{
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(-s2, -s2, -s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(s2, -s2, -s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(-s2, s2, -s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(s2, s2, -s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(-s2, -s2, s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(s2, -s2, s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(-s2, s2, s2)), s2);
      trace_point_cloud(ctx, vec3_sub(position, vec3_new(s2, s2, s2)), s2);
      
    }
  }
}

lisp_value sft_poly(){
  int count = 0;
  void emit_pt(vec3 p, vec3 c){
    count += 1;
  }
  
  df_ctx c = {.sdf = tree/*d1*/, .emit_point = emit_pt, .threshold = 0.3};
  trace_point_cloud(&c, vec3_new(0,0,0), 5.0);
  var vec = make_vector(integer(count * 3), float32(0.0));
  f32 * verts = vec.vector->data;
  var vec2 = make_vector(integer(count * 3), float32(0.0));
  f32 * colors = vec2.vector->data;
  
  int i = 0;
  int i2 = 0;
  void emit_pt2(vec3 p, vec3 c){
    verts[i++] = p.x;
    verts[i++] = p.y;
    verts[i++] = p.z;
    colors[i2++] = c.x;
    colors[i2++] = c.y;
    colors[i2++] = c.z;
  }
  c.emit_point = emit_pt2;
  
  trace_point_cloud(&c, vec3_new(0,0,0), 5.0);

  trace_point(&c, vec3_new(0,0,0), 0.1);

  trace_point(&c, vec3_new(1,0,0), 0.1);
  trace_point(&c, vec3_new(5,5,0), 0.1);

  vec3 p = trace_point(&c, vec3_new(0,10,0), 0.1);
  //var g = sdf_gradient(&c, p, 0.01);

  p = trace_point(&c, p, 0.1);
  var g = sdf_gradient(&c, p, 0.1);
      
  var s1 = vec3_new(g.y, g.x, -g.z);;
  
  var s2 = vec3_mul_cross(g, s1);
  printf("POINTS: %i\n", count);
  return new_cons(vec, vec2);
}

void test_sdf(){
  sft_poly();
}

