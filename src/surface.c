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

f32 tree(void * userdata, vec3 v, vec3 * c){
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
 
  
  if(d == d1){
    rgb color1 = {.r = 0.7, .g = 0.4, .b = 0.4};
    rgb color2 = {.r = 0.6, .g = 0.5, .b = 0.35};
    f32 n = noise(vec2_new(v.x,v.z));
        
    var col = rgb_blend(color1, color2, n);
    c->x = col.r;
    c->y = col.g;
    c->z = col.b;
  }else if(d == dg){
    var rgb = get_color(vec2_new(v.x, v.y));;
    *c = vec3_new(rgb.r, rgb.g, rgb.b);
    //*c = vec3_new(0.5,0.8,0.5);
  }
  return d;
}



typedef struct{
  f32 (* sdf)(void * userdata, vec3 v, vec3 * color);
  void (* emit_point)(void * userdata, vec3 pt, vec3 color);
  f32 threshold;
  void * userdata;
  void * sdf_userdata;
}df_ctx;

vec3 sdf_gradient(df_ctx * ctx, vec3 pt, f32 size){
  vec3 c;
  var ptx = pt;
  var pty = pt;
  var ptz = pt;
  ptx.x += size * 0.2;
  pty.y += size * 0.2;
  ptz.z += size * 0.2;
  var dx1 = ctx->sdf(ctx->sdf_userdata, ptx, &c);
  var dy1 = ctx->sdf(ctx->sdf_userdata, pty, &c);
  var dz1 = ctx->sdf(ctx->sdf_userdata, ptz, &c);

  ptx.x -= size * 0.2 * 2;
  pty.y -= size * 0.2 * 2;
  ptz.z -= size * 0.2 * 2;
  var dx2 = ctx->sdf(ctx->sdf_userdata, ptx, &c);
  var dy2 = ctx->sdf(ctx->sdf_userdata, pty, &c);
  var dz2 = ctx->sdf(ctx->sdf_userdata, ptz, &c);

  var x = vec3_normalize(vec3_new(dx1 - dx2, dy1 - dy2, dz1 - dz2));
  return x;
}

vec3 trace_point(df_ctx * ctx, vec3 pt, f32 size){
  var x = sdf_gradient(ctx, pt, size);
  vec3 c;
  var d0 = ctx->sdf(ctx->sdf_userdata, pt, &c);

  var r = vec3_sub(pt, vec3_scale(x, d0));
  return r;
}

// idea: use the normal to reduce the number of planes.
void trace_point_cloud(df_ctx * ctx, vec3 position, f32 size){
  f32 d;
  vec3 c;
  d = ctx->sdf(ctx->sdf_userdata, position, &c);
  if(fabs(d) < size * 1.5 ){
    f32 s2 = size * 0.5;
    
    if(size < ctx->threshold * 1.42){
      size = size * 0.5;
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

        ctx->sdf(ctx->sdf_userdata, p1, &c);
        
        ctx->emit_point(ctx->userdata, p1, c);
        ctx->sdf(ctx->sdf_userdata, p2, &c);
        
        ctx->emit_point(ctx->userdata, p2, c);
        ctx->sdf(ctx->sdf_userdata, p4, &c);
        ctx->emit_point(ctx->userdata, p4, c);
        ctx->sdf(ctx->sdf_userdata, p2, &c);
        
        ctx->emit_point(ctx->userdata, p2, c);
        ctx->sdf(ctx->sdf_userdata, p3, &c);
        ctx->emit_point(ctx->userdata, p3, c);
        ctx->sdf(ctx->sdf_userdata, p4, &c);
        ctx->emit_point(ctx->userdata, p4, c);
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

typedef struct{
  int count;
  int i;
  f32 * verts;
  f32 * colors;
}sft_context;

void emit_pt2(void * userdata, vec3 p, vec3 c){
  sft_context * id = userdata;
  var i = id->i * 3;
  id->verts[i] = p.x;
  id->verts[i + 1] = p.y;
  id->verts[i + 2] = p.z;
  id->colors[i] = c.x;
  id->colors[i + 1] = c.y;
  id->colors[i + 2] = c.z;
  id->i += 1;
}

static void emit_pt(void * userdata, vec3 p, vec3 c){
  sft_context * ud = userdata;
  ud->count += 1;
}

f32 lisp_sdf_func(void * data, vec3 pt, vec3 * color){
  lisp_value * v = data;
  var f1 = car(*v);
  lisp_vector vec;
  vec.data = pt.data;
  vec.count = 3;
  vec.elem_size = 4;
  vec.default_value.type = LISP_FLOAT32;
  lisp_value vec2 = vector_lisp_value(&vec);

  cons a,b;
  
  lisp_value r = lisp_eval(lisp_get_root_scope(), new_stack_cons(&a, f1, new_stack_cons(&b, vec2, nil)));
  var d = lisp_value_rational(r);

  var f2 = cdr(*v);

  lisp_value r2 = lisp_eval(lisp_get_root_scope(), new_stack_cons(&a, f2, new_stack_cons(&b, vec2, nil)));
  f32 * f = r2.vector->data;
  color->x = f[0];
  color->y = f[1];
  color->z = f[2];
  
  return d;
  
}

lisp_value sdf_poly(lisp_value f){

  sft_context ctx = {0};
  
  df_ctx c = {.sdf = tree/*d1*/, .userdata = &ctx, .emit_point = emit_pt, .threshold = 0.25};
  if(!is_nil(f)){
    c.sdf = lisp_sdf_func;
    c.sdf_userdata = &f;
  }
  trace_point_cloud(&c, vec3_new(0,0,0), 5.0);
  var vec = make_vector(integer(ctx.count * 3), float32(0.0));
  f32 * verts = vec.vector->data;
  var vec2 = make_vector(integer(ctx.count * 3), float32(0.0));
  f32 * colors = vec2.vector->data;
  ctx.colors = colors;
  ctx.verts = verts;
  c.emit_point = emit_pt2;
  trace_point_cloud(&c, vec3_new(0,0,0), 5.0);
  printf("POINTS: %i\n", ctx.count);
  return new_cons(vec, vec2);
}

void test_sdf(){
  sdf_poly(nil);
}

