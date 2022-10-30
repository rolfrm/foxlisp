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
  var dv = vec3_new(dx1 - dx2, dy1 - dy2, dz1 - dz2);
  var l = vec3_len(dv);
  if(fabs(l) < 0.00001)
    return vec3_zero;
  var x = vec3_scale(dv, 1.0 / l);
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


typedef struct{
  f32 (* sdf1)(void * userdata, vec3 v, vec3 * color);
  f32 (* sdf2)(void * userdata, vec3 v, vec3 * color);
  f32 threshold;
  void * userdata1;
  void * userdata2;
  vec3 pt;
  bool collision_detected;
  f32 greatest_common_overlap;
  int iterations;
}cdf_ctx;

const float sqrt_3 = 1.73205; 

void sdf_detect_collision(cdf_ctx * ctx, vec3 position, f32 size){
  if(ctx->collision_detected) return;
  var d = ctx->sdf1(ctx->userdata1, position, NULL);
  var d2 = ctx->sdf2(ctx->userdata2, position, NULL);
  //vec3_print(position);
  //printf("%f %f %f\n", d, d2, size);
  if(d < size * sqrt_3 && d2 < size * sqrt_3 ){
    f32 s2 = size * 0.5;
    
    if(size < ctx->threshold * sqrt_3){
      // collison detected
      ctx->pt = position;
      ctx->collision_detected = MAX(d, d2) < -ctx->threshold;
      return;
    }
    else{
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(-s2, -s2, -s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(s2, -s2, -s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(-s2, s2, -s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(s2, s2, -s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(-s2, -s2, s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(s2, -s2, s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(-s2, s2, s2)), s2);
      sdf_detect_collision(ctx, vec3_sub(position, vec3_new(s2, s2, s2)), s2);
      
    }
  }
}

typedef struct{
  f32 d, d2;
  i32 i;
}fi_pair;

int sort_pairs(const fi_pair * a, const fi_pair * b){
  return a->d > b->d ? 1 : -1;
}

void sdf_detect_max_overlap(cdf_ctx * ctx, vec3 position, f32 size){
  // minimize max(d(p), d2(p))
  ctx->iterations += 1;
  
  if(size < ctx->threshold * 1.7){
    var d = ctx->sdf1(ctx->userdata1, position, NULL);
    var d2 = ctx->sdf2(ctx->userdata2, position, NULL);
    var min_d = d - size * 1.73 * 0.5;
    var min_d2 = d2 - size * 1.73 * 0.5;
  

    // collison detected
    ctx->pt = position;
    ctx->collision_detected = true;
    ctx->greatest_common_overlap = MAX(min_d, min_d2);
    return;
  }
    
    else{
      f32 s2 = size * 0.5;
      
      fi_pair candidates[8] = {0};
      f32 o[] = {-s2, s2};
      for(int i = 0; i < 8; i++){
        vec3 offset = vec3_new(o[i&1], o[(i>>1)&1], o[(i>>2)&1]);
        
        var p = vec3_add(offset, position);
        var d = ctx->sdf1(ctx->userdata1, p, NULL);
        var d2 = ctx->sdf2(ctx->userdata2, p, NULL);
        var min_d = d ;
        var min_d2 = d2;
        candidates[i] = (fi_pair){
          .d = MAX(min_d2, min_d),
          .d2 = MIN(min_d2, min_d),
          .i = i};
        
      }
      qsort(candidates, 8, sizeof(fi_pair), (void *) sort_pairs);
      
      for(int _i = 0; _i < 8; _i++){
        int i = candidates[_i].i;
        var d = candidates[i].d;
        if(d - s2 * 1.73 * 0.5 > ctx->greatest_common_overlap){
          break;
        }
        
        vec3 offset = vec3_new(o[i&1], o[(i>>1)&1], o[(i>>2)&1]);
        var p = vec3_add(offset, position);
        sdf_detect_max_overlap(ctx, p, s2);
      }      
    }
}


static vec3 lv_vec3(lisp_value v){
  return vec3_new(
                  lisp_value_as_rational(car(v)),
                  lisp_value_as_rational(cadr(v)),
                  lisp_value_as_rational(caddr(v)));
                  
}

typedef enum{
  SDF_TYPE_UNRECOGNIZED,
  SDF_TYPE_SPHERE,
  SDF_TYPE_SPHERE_BOUNDS,
  SDF_TYPE_AABB,
  SDF_TYPE_VERT_CAPSULE,
  SDF_TYPE_TRANSFORM,
  SDF_TYPE_MODELS,
  SDF_TYPE_COLOR,
  SDF_TYPE_SDFBM,
  SDF_TYPE_SOFT,
  SDF_TYPE_SUBTRACT
}sdf_type;

typedef struct{
  sdf_type type;
  vec3 pos;
  vec3 size;
}sdf_aabb;

typedef struct{
  sdf_type type;
  vec3 pos;
  f32 radius;
}sdf_sphere;

typedef struct{
  sdf_type type;
  f32 radius;
  f32 height;
  vec3 pos;

}sdf_vert_capsule;
//f32 d1 = vert_capsule(vec3_sub(v, vec3_new(0,0,0)), 3.0, 0.5);

typedef struct {
  sdf_type type;
  void ** models;
  size_t model_count;
  mat4 inv_tform;
}sdf_transform;

typedef struct {
  sdf_type type;
  void ** models;
  size_t model_count;
}sdf_models;

typedef struct {
  sdf_type type;
  void ** models;
  size_t model_count;
  vec3 color;

}sdf_color;

typedef struct {
  sdf_type type;
  void ** models;
  size_t model_count;
  f32 soft;

}sdf_soft;


typedef struct{
  sdf_type type;
  void ** models;
  size_t model_count;
  vec3 pos;
  f32 radius;
  
}sdf_sphere_bounds;



typedef struct {
  sdf_type type;
  void ** models;
  size_t model_count;
  f32 k;

}sdf_subtract;


typedef struct{
  int seed;
  int octaves;
  vec3 offset;
}sdf_noisefield;


f32 generic_sdf(void * ud, vec3 p, vec3 * c);
f32 models_sdf(void * ud, vec3 p, vec3 * color);

f32 sphere_sdf(void * ud, vec3 p, vec3 * c){
  UNUSED(c);
  sdf_sphere * a = ud;
  return vec3_len(vec3_sub(p, a->pos)) - a->radius;
}


f32 sphere_bounds_sdf(void * ud, vec3 p, vec3 * c){
  UNUSED(c);
  sdf_sphere_bounds * a = ud;
  var d = vec3_len(vec3_sub(p, a->pos)) - a->radius * 1.5;
  if(d < 0){
  
    return models_sdf(ud, p, c);
  }
  //return models_sdf(ud, p, c);
  return d + a->radius * 0.5;
}


f32 aabb_sdf(void * ud, vec3 p, vec3 * color){
  UNUSED(color);
  sdf_aabb * a = ud;

  p = vec3_sub(p, a->pos);
  vec3 q = vec3_sub(vec3_abs(p), a->size);
  return vec3_len(vec3_max(q, vec3_zero)) + MIN(MAX(q.x,MAX(q.y,q.z)),0.0);
}

f32 vert_capsule_sdf(void * ud, vec3 p, vec3 * c){
  UNUSED(c);
  sdf_vert_capsule * a = ud;
  return vert_capsule(vec3_sub(p, a->pos), a->height, a->radius);
}

f32 transform_sdf(void * ud, vec3 p, vec3 * color){
  sdf_transform * a = ud;
  p = mat4_mul_vec3(a->inv_tform, p);

  //vec3 x1 = mat4_mul_vec3(a->inv_tform, vec3_new(0,0,0));
  //vec3 x2 = mat4_mul_vec3(a->inv_tform, vec3_new(1,1,1));
  //f32 eig = vec3_len(vec3_sub(x2, x1)) / sqrtf(3);
  return models_sdf(ud, p, color);// / eig;
}

f32 models_sdf(void * ud, vec3 p, vec3 * color){
  sdf_models * models = ud;
  f32 d = 100000;
  vec3 c2 = color != NULL ? *color : vec3_zero;
  vec3 * c2p = color != NULL ? &c2 : NULL;
  for(var i = 0; i < models->model_count; i++){
    var d1 = generic_sdf(models->models[i], p, c2p);
    if(d1 < d){
      d = d1;
      if(color != NULL)
        *color = c2;
    }
  }
  return d;
}
f32 color_sdf(void * ud, vec3 p, vec3 * color){
  sdf_color * color_elem = ud;
  if(color != NULL){
    *color = color_elem->color;
  }
  return models_sdf(ud, p, color);
}

f32 soft_sdf(void * ud, vec3 p, vec3 * color){
  sdf_soft * soft = ud;
  
  return models_sdf(ud, p, color) - soft->soft;
}
//x×(1−a)+y×a. 
f32 mixf(f32 x, f32 y, f32 a){
  return x * (1 - a) + y * a;
}
//float opSmoothSubtraction( float d1, float d2, float k ) {
//    float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
//    return mix( d2, -d1, h ) + k*h*(1.0-h); }

f32 subtract_sdf(void * ud, vec3 p, vec3 * color){
  sdf_subtract * soft = ud;
  var k = soft->k;
  if(soft->model_count == 0) return 1000000;
  var d1 = generic_sdf(soft->models[0],p, color);
  for(int i = 1; i < soft->model_count; i++){
    var d2 = generic_sdf(soft->models[i],p, NULL);
    float h = CLAMP( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
    d1 = mixf( d2, -d1, h ) + k*h*(1.0-h); 
  }

  return d1;
}


vec3 * color_out = NULL;
f32 generic_sdf(void * ud, vec3 p, vec3 * c){
  sdf_type * tp = ud;
  switch(tp[0]){
  case SDF_TYPE_TRANSFORM:
    return transform_sdf(ud, p, c);
  case SDF_TYPE_AABB:
    return aabb_sdf(ud, p, c);
  case SDF_TYPE_SPHERE:
    return sphere_sdf(ud, p, c);
  case SDF_TYPE_VERT_CAPSULE:
    return vert_capsule_sdf(ud, p,c);
  case SDF_TYPE_MODELS:
    return models_sdf(ud, p, c);
  case SDF_TYPE_COLOR:
    return color_sdf(ud, p, c);
  case SDF_TYPE_SOFT:
    return soft_sdf(ud, p, c);
  case SDF_TYPE_SUBTRACT:
    return subtract_sdf(ud, p, c);
  case SDF_TYPE_SPHERE_BOUNDS:
    return sphere_bounds_sdf(ud, p, c);
  default:;
  }
  printf("Unrecognized SDF type\n");
  return 1000000.0;
}


lisp_value sdf_lookup;
static void * get_physics_sdf(lisp_value value){
  println(value);
  void ** models = NULL;
  size_t model_count = 0;
  while(is_cons(value)){
    var tform = caar(value);
    mat4 * m = lisp_value_vector(tform)->data;
    
    sdf_transform a_t = {
      .type = SDF_TYPE_TRANSFORM,
      .inv_tform = mat4_invert(*m),
      .models = alloc0(sizeof(void *)),
      .model_count = 0
    };
    
    var model_type = cdar(value);
    println(model_type);
    if(lisp_value_eq(car(model_type), get_symbol("aabb"))){
      vec3 s = lv_vec3(cdr(model_type));

      sdf_aabb * aabb = alloc(sizeof(*aabb));
      aabb->type = SDF_TYPE_AABB;
      aabb->pos = vec3_zero;
      aabb->size = s;
      vec3_print(s);printf("<--\n");
      a_t.models[0] = aabb;
      a_t.model_count = 1;   
    }
    if(lisp_value_eq(car(model_type), get_symbol("sphere"))){
      f32 s = lisp_value_rational(cadr(model_type));

      sdf_sphere * sphere = alloc(sizeof(*sphere));

      sphere->type = SDF_TYPE_SPHERE;
      sphere->pos = vec3_zero;
      sphere->radius = s;
      a_t.models[0] = sphere;
      a_t.model_count = 1;
    }
    if(lisp_value_eq(car(model_type), get_symbol("capsule"))){
      f32 h = lisp_value_rational(cadr(model_type));
      f32 r = lisp_value_rational(caddr(model_type));

      sdf_vert_capsule * capsule = alloc(sizeof(*capsule));

      capsule->type = SDF_TYPE_VERT_CAPSULE;
      capsule->radius = r;
      capsule->height = h;
      a_t.models[0] = capsule;
      a_t.model_count = 1;
    }
        
    if(a_t.model_count > 0){
      models = realloc(models, sizeof(void *) * ++model_count);
      models[model_count-1] = iron_clone(&a_t, sizeof(a_t));
    }
    value = cdr(value);
  }
  if(models == NULL) return NULL;
  if(model_count == 1) {
    var r = models[0];
    dealloc(models);
    return r;
  }
  return models;
}

static void * get_physics_sdf2(lisp_value value){
  var model_type = car(value);
  if(lisp_value_eq(model_type, get_symbol("transform"))){
    
    sdf_transform * transform = alloc(sizeof(*transform));
    transform->type = SDF_TYPE_TRANSFORM;
    mat4 * m = lisp_value_vector(cadr(value))->data;
    transform->inv_tform = mat4_invert(*m);
    value = cddr(value);
    transform->model_count = lisp_length(value).integer;
    transform->models = alloc0(sizeof(void *) * transform->model_count);
    for(size_t i = 0; i < transform->model_count; i++){
      transform->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return transform;
  }
  if(lisp_value_eq(model_type, get_symbol("rgb"))){
    
    sdf_color * rgb = alloc(sizeof(*rgb));
    rgb->type = SDF_TYPE_COLOR;
    rgb->color = lv_vec3(cadr(value));
    value = cddr(value);
    
    rgb->model_count = lisp_length(value).integer;
    rgb->models = alloc0(sizeof(void *) * rgb->model_count);
    for(size_t i = 0; i < rgb->model_count; i++){
      rgb->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return rgb;
  }
  if(lisp_value_eq(model_type, get_symbol("soft"))){
    
    sdf_soft * soft = alloc(sizeof(*soft));
    soft->type = SDF_TYPE_SOFT;
    soft->soft = lisp_value_as_rational(cadr(value));
    value = cddr(value);
    
    soft->model_count = lisp_length(value).integer;
    soft->models = alloc0(sizeof(void *) * soft->model_count);
    for(size_t i = 0; i < soft->model_count; i++){
      soft->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return soft;
  }
  
  if(lisp_value_eq(model_type, get_symbol("subtract"))){
    
    sdf_subtract * subtract = alloc(sizeof(*subtract));
    subtract->type = SDF_TYPE_SUBTRACT;
    subtract->k = lisp_value_as_rational(cadr(value));
    value = cddr(value);
    
    subtract->model_count = lisp_length(value).integer;
    subtract->models = alloc0(sizeof(void *) * subtract->model_count);
    for(size_t i = 0; i < subtract->model_count; i++){
      subtract->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return subtract;
  }
  if(lisp_value_eq(model_type, get_symbol("aabb"))){
    vec3 s = lv_vec3(cdr(value));

    sdf_aabb * aabb = alloc(sizeof(*aabb));
    aabb->type = SDF_TYPE_AABB;
    aabb->pos = vec3_zero;
    aabb->size = s;
    return aabb;
  }
  if(lisp_value_eq(model_type, get_symbol("sphere"))){
    f32 s = lisp_value_rational(cadr(value));
    
    sdf_sphere * sphere = alloc(sizeof(*sphere));
    
    sphere->type = SDF_TYPE_SPHERE;
    sphere->pos = vec3_zero;
    sphere->radius = s;
    return sphere;
  }
  
  if(lisp_value_eq(model_type, get_symbol("sphere-bounds"))){
    
    sdf_sphere_bounds * sphere = alloc(sizeof(*sphere));
    
    sphere->type = SDF_TYPE_SPHERE_BOUNDS;
    sphere->pos = vec3_zero;
    sphere->radius = 0.0;

    value = cdr(value);
    sphere->model_count = lisp_length(value).integer;
    sphere->models = alloc0(sizeof(void *) * sphere->model_count);
    for(size_t i = 0; i < sphere->model_count; i++){
      sphere->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }

    
    vec3 check_points[] = {
      vec3_new(1,1,1),
      vec3_new(-1,1,1),
      vec3_new(1,-1,1),
      vec3_new(-1,-1,1),
      vec3_new(1,1,-1),
      vec3_new(-1,1,1),
      vec3_new(1,-1,-1),
      vec3_new(-1,-1,-1)

    };
    f32 d[8];
    vec3 c;
    vec3 center = vec3_zero;
    for(int i = 0; i < 8; i++)
      check_points[i] = vec3_scale(check_points[i], 5000);
      
    
    for(int i = 0; i < 8; i++){
      vec3 dir = vec3_normalize(vec3_sub(center, check_points[i]));
      f32 maxd = 100000.0;
      for(size_t j = 0; j < sphere->model_count; j++){
        f32 d2 = generic_sdf(sphere->models[j], check_points[i], &c);
        if(d2 < maxd)
          maxd = d2;
      }
      d[i] = maxd;
      check_points[i] = vec3_add(check_points[i], vec3_scale(dir, d[i] ));
      center = vec3_add(center, vec3_scale(check_points[i], 1.0 / 8.0));
    }
    sphere->pos = center;
    sphere->radius = 0.0;
    for(int i = 0; i < 8; i++)
      sphere->radius = fmax(sphere->radius, vec3_len(vec3_sub(check_points[i], center)));
        return sphere;
  }

  if(lisp_value_eq(model_type, get_symbol("capsule"))){
    f32 h = lisp_value_rational(cadr(model_type));
    f32 r = lisp_value_rational(caddr(model_type));
    
    sdf_vert_capsule * capsule = alloc(sizeof(*capsule));
    
    capsule->type = SDF_TYPE_VERT_CAPSULE;
    capsule->radius = r;
    capsule->height = h;
    return capsule;
  }
  
  size_t count = lisp_length(cdr(value)).integer;
  if(count == 0)
    return NULL;
  if(count == 1)
    return get_physics_sdf2(cadr(value));
  sdf_models * mod = alloc0(sizeof(*mod));
  mod->type = SDF_TYPE_MODELS;
  mod->model_count = count;
  mod->models = alloc0(sizeof(void *) * mod->model_count);
  for(size_t i = 0; i < mod->model_count; i++){
    mod->models[i] = get_physics_sdf2(car(value));
    value = cdr(value);
  }
  return mod;
}


void describe_sdf(void * ptr){
  sdf_type * tp = ptr;
  switch(tp[0]){
  case SDF_TYPE_TRANSFORM:{
    sdf_transform * t = ptr;
    printf("Transform: ");
    mat4_print(t->inv_tform);
    for(size_t i = 0; i < t->model_count; i++)
      describe_sdf(t->models[i]);
    break;
  }
  case SDF_TYPE_AABB:{
    sdf_aabb * t = ptr;
    printf("AABB ");vec3_print(t->pos);vec3_print(t->size);printf("\n");
    break;
  }
   case SDF_TYPE_SPHERE:{
     sdf_sphere * t = ptr;
     printf("SPHERE %f\n", t->radius);
     break;
   }
  case SDF_TYPE_SPHERE_BOUNDS:{
     sdf_sphere_bounds * t = ptr;
     printf("SPHERE Bounds %f  ", t->radius); vec3_print(t->pos);
     printf("\n");
     break;
   }
  case SDF_TYPE_VERT_CAPSULE:{
     sdf_vert_capsule * t = ptr;
     printf("CAPSULE r=%f h=%f\n", t->radius, t->height);
     break;
   }
  case SDF_TYPE_MODELS:{
    
    sdf_models * m = ptr;
    printf("Models: %i\n", (i32)m->model_count);
    for(size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  case SDF_TYPE_COLOR:{
    
    sdf_color * m = ptr;
    printf("COLOR: %i\n", (i32)m->model_count);
    for(size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
    case SDF_TYPE_SOFT:{
    
    sdf_soft * m = ptr;
    printf("SOFT: %f\n", m->soft);
    for(size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
    }
    case SDF_TYPE_SUBTRACT:{
    
      sdf_subtract * m = ptr;
      printf("SUBTRACT: %f\n", m->k);
      for(size_t i = 0; i < m->model_count; i++)
        describe_sdf(m->models[i]);
      break;
    }
  default:
    printf("EEERR\n");
  }
}
void * get_physics_model_cached(lisp_value physics1){
   if(is_nil(sdf_lookup)){
     sdf_lookup = lisp_make_hashtable(NULL, 0);
     lisp_register_value("++sdf-lookup", sdf_lookup);
  }
   if(!is_nil(physics1)){
    var thing = lisp_hashtable_get(sdf_lookup, physics1);
    if(is_nil(thing)){
      void * sdf = get_physics_sdf(physics1);
      if(sdf == NULL)
        thing = integer_lisp_value(0);
      else
        thing = native_pointer_lisp_value(sdf);
      lisp_hashtable_set(sdf_lookup, physics1, thing);
    }
    return thing.pointer;
    
  }
   return NULL;
}
void * get_physics_model_cached2(lisp_value physics1){
   if(is_nil(sdf_lookup)){
     sdf_lookup = lisp_make_hashtable(NULL, 0);
     lisp_register_value("++sdf-lookup", sdf_lookup);
   }
   if(!is_nil(physics1)){
    var thing = lisp_hashtable_get(sdf_lookup, physics1);
    if(is_nil(thing)){
      void * sdf = get_physics_sdf2(physics1);
      printf("Physics model: %p\n", sdf);
      if(sdf == NULL)
        thing = integer_lisp_value(0);
      else
        thing = native_pointer_lisp_value(sdf);
      lisp_hashtable_set(sdf_lookup, physics1, thing);
    }
    return thing.pointer;
    
  }
   return NULL;
}

lisp_value foxgl_detect_collision(lisp_value obj1, lisp_value obj2,lisp_value physics1, lisp_value physics2, lisp_value out_cons){
 
  var p1 = lv_vec3(car(obj1));
  var p2 = lv_vec3(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  var rot1 = lisp_value_as_rational(car(obj1));
  var rot2 = lisp_value_as_rational(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  sdf_aabb a = {
    .type = SDF_TYPE_AABB,
    .pos = vec3_zero,
    .size = vec3_new(0.25, 05, 0.25)
  };
  sdf_aabb b = {
    .type = SDF_TYPE_AABB,
    .pos = vec3_zero,
    .size = vec3_new(0.25,0.5,0.25)
  };
  void * m1[] = {&a};
  sdf_transform a_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot1),
                                         -p1.x, -p1.y, -p1.z ),
    .models = m1,
    .model_count = 1};
  
  void * ptr1 = get_physics_model_cached(physics1);
  if(ptr1 == NULL)
    ptr1 = &a;

  m1[0] = ptr1;
  
  void * m2[] = {&b};
  
  sdf_transform b_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot2),
                                         -p2.x,-p2.y,-p2.z ),
    .models = m2,
    .model_count = 1
  };
  void * ptr2 = get_physics_model_cached(physics2);
    
  if(ptr2 == NULL)
    ptr2 = &b;
  m2[0] = ptr2;
  
  cdf_ctx ctx = {
    .sdf1 = transform_sdf,
    .sdf2 = transform_sdf,
    .userdata1 = &a_t,
    .userdata2 = &b_t,
    .threshold = 0.01,
    .greatest_common_overlap = 100.0
  };
  cdf_ctx ctx2 = ctx;
  sdf_detect_collision(&ctx, p2, 10.0);
  if(ctx.collision_detected && is_cons(out_cons)){
    ctx2.threshold = 0.01;
    //describe_sdf(&a_t);
    //describe_sdf(&b_t);
    
    describe_sdf(ctx.userdata1);
    describe_sdf(ctx.userdata2);
    sdf_detect_max_overlap(&ctx2, p2, 10.0);
    set_car(out_cons, rational_lisp_value(ctx2.pt.x));
    set_cdr(out_cons, rational_lisp_value(ctx2.pt.z));

  }
  return ctx.collision_detected ? t : nil;
}


lisp_value foxgl_detect_collision_floor(lisp_value floor_tile, lisp_value obj2, lisp_value model){
  var p1l = car(floor_tile);
  var p1 = vec3_new(lisp_value_as_rational(car(p1l)), 0, lisp_value_as_rational(cadr(p1l)));
  var p2 = lv_vec3(car(obj2));
  floor_tile = cdr(floor_tile);
  obj2 = cdr(obj2);
  var rot2 = lisp_value_as_rational(car(obj2));
  obj2 = cdr(obj2);
  var s1l = car(floor_tile);

  void * ptr2 = get_physics_model_cached(model);
  sdf_aabb b = {.pos = p2, .size = vec3_new(0.1, 0.1, 0.1)};
  if(ptr2 == NULL)
    ptr2 = &b;
  void * m1[] = {ptr2};
  sdf_transform b_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot2),
                                         -p2.x, -p2.y, -p2.z ),
    .models = m1,
    .model_count = 1
  };

  
  var size = vec3_new(lisp_value_as_rational(car(s1l)), 10.0, lisp_value_as_rational(cadr(s1l)));
  sdf_aabb a = {
    .type = SDF_TYPE_AABB,
    .pos = p1, .size = vec3_sub(size, vec3_new(0.5,0.5,0.5))};
  cdf_ctx ctx = {
    .sdf1 = aabb_sdf,
    .sdf2 = transform_sdf,
    .userdata1 = &a,
    .userdata2 = &b_t,
    .threshold = 0.01,
    .greatest_common_overlap = 100.0
  };

  sdf_detect_collision(&ctx, p2, 10.0);
  
  return ctx.collision_detected ? t : nil;
}


void test_sdf_col(){
  
  sdf_sphere s1 = {.pos = vec3_new(-0.1, -0.1, 0), .radius = 1.0 };
  sdf_sphere s2= {.pos = vec3_new(1.599, 0, 0), .radius = 1.0 };
  cdf_ctx ctx = {
    .sdf1 = sphere_sdf,
    .sdf2 = sphere_sdf,
    .userdata1 = &s1,
    .userdata2 = &s2,
    .threshold = 0.01,
    .greatest_common_overlap = 10.0
  };
  sdf_detect_max_overlap(&ctx, vec3_new(2,2,2), 10.0);
  var truth = vec3_len(vec3_sub(s1.pos, s2.pos)) - s1.radius  - s2.radius;
  
  
  printf("MAx overlap: %f == %f %i? \n", ctx.greatest_common_overlap, truth, ctx.iterations);
  if(ctx.greatest_common_overlap > 0.0){
    return;
  }
  //return;
  
  {
  sdf_aabb a = {.pos = vec3_new(0, 0, 0), .size = vec3_new(0.5,0.5,0.5)};
  sdf_aabb b = {.pos = vec3_new(0.9, 0, 0.), .size = vec3_new(0.5, 0.5, 0.5)};
  cdf_ctx ctx = {
    .sdf1 = aabb_sdf,
    .sdf2 = aabb_sdf,
    .userdata1 = &a,
    .userdata2 = &b,
    .threshold = 0.01,
    .greatest_common_overlap = 10
  };
  sdf_detect_max_overlap(&ctx, vec3_new(0,0,0), 10.0);
  var truth = vec3_len(vec3_sub(s1.pos, s2.pos)) - s1.radius  - s2.radius;

  
  printf("MAx overlap: %f == %f %i? \n", ctx.greatest_common_overlap, truth, ctx.iterations);

  }
  
}

void test_layered_sdf(){
  {
  mat4 t = mat4_translate(1.0, 0, 0);
  mat4 s = mat4_scaled(0.2, 0.2, 0.2);
  mat4 t_inv = mat4_invert(mat4_mul(t, s));
  mat4_print(mat4_invert(t_inv));
  sdf_sphere s1 = {.type = SDF_TYPE_SPHERE, .pos = vec3_zero, .radius = 1.0};
  
  void * m1[] = {&s1};
  sdf_transform t1 = {.type = SDF_TYPE_TRANSFORM, .inv_tform = t_inv,
    .models = m1, .model_count = 1};

  // a sphere size 0.1 centered at 1.0.
  sdf_sphere s2 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(1.0, 0, 0), .radius = 0.2};
  for(float x = -1; x < 3; x += 0.1){
    for(float y = -1; y < 3; y += 0.1){
      
    var pt = vec3_new(x, 0.1, y);
    var a = generic_sdf(&t1, pt, NULL);
    var b = generic_sdf(&s2, pt, NULL);
    printf("x: %f   : %f == %f: %f\n", x, a, b, a - b);
    }
  }
  }
  {
    printf("AABB\n");
    mat4 t = mat4_translate(1.0, 0, 0);
    mat4 s = mat4_scaled(0.2, 0.2, 0.2);
    mat4 t_inv = mat4_invert(mat4_mul(t, s));
    mat4_print(mat4_invert(t_inv));
    sdf_aabb s1 = {.type = SDF_TYPE_AABB, .pos = vec3_zero, .size = vec3_one};
    void * m1[] = {&s1};
  
    sdf_transform t1 = {.type = SDF_TYPE_TRANSFORM, .inv_tform = t_inv,
      .models = m1, .model_count = 1};
    
    // a sphere size 0.1 centered at 1.0.
    sdf_aabb s2 = {.type = SDF_TYPE_AABB, .pos = vec3_new(1.0, 0, 0), .size = vec3_new(0.2, 0.2, 0.2)};
    for(float x = -1; x < 3; x += 0.1){
      for(float y = -1; y < 3; y += 0.1){
        
        var pt = vec3_new(x, 0.1, y);
        var a = generic_sdf(&t1, pt, NULL);
        var b = generic_sdf(&s2, pt, NULL);
        printf("x: %f   : %f == %f: %f\n", x, a, b, a - b);
      }
    } 
  }
}

#include "mc.h"

void print_triangle(void * ud, vec3 v1, vec3 v2, vec3 v3){

}


void marching_cubes_emit_point(void * userdata, vec3 a, vec3 b, vec3 c){
  df_ctx * ctx = userdata;
  vec3 color = vec3_new(0,0,1);
  /*for(int i = 0; i < 1; i++){
    a = trace_point(ctx, a, 0.001);
    b = trace_point(ctx, b, 0.001);
    c = trace_point(ctx, c, 0.001);
    }*/
  
  //ctx->sdf(ctx->sdf_userdata, a, &color);
  
  ctx->emit_point(ctx->userdata, a, color);
  
  //ctx->sdf(ctx->sdf_userdata, b, &color);
  ctx->emit_point(ctx->userdata, b, color);
  
  //ctx->sdf(ctx->sdf_userdata, c, &color);
  ctx->emit_point(ctx->userdata, c, color);
}


f32 marching_cubes_sdff(void * userdata, vec3 pt, vec3 * color){
  df_ctx * ctx = userdata;
  return ctx->sdf(ctx->sdf_userdata, pt, color);
}

void marching_cubes_sdf(df_ctx * ctx, vec3 position, f32 size){
  f32 d;
  vec3 c;
  d = ctx->sdf(ctx->sdf_userdata, position, &c);
  if(d < size * sqrt_3 ){
    if(size <= ctx->threshold){
      sdf_model model = {
        .sdf = marching_cubes_sdff,
        .userdata = ctx,
        .threshold = 0.01
      };
      process_cube(&model, position, size, marching_cubes_emit_point, ctx);
    }
    else{
      
      f32 s2 = size * 0.5;
      f32 o[] = {-s2, s2};
      for(int i = 0; i < 8; i++){
        vec3 offset = vec3_new(o[i&1], o[(i>>1)&1], o[(i>>2)&1]);
        var p = vec3_add(offset, position);
        marching_cubes_sdf(ctx, p, s2);
      }
    }
  }
}

void mc_count_vertexes(void * userdata, vec3 pt, vec3 color){
  UNUSED(pt);
  UNUSED(color);
  size_t * c = userdata;
  c[0] += 1;
}

typedef struct{
  f32 * verts;
  f32 * colors;
  size_t count;
  size_t offset;
  int dupcnt;
  df_ctx * sdf;

  f32 * out_verts;
  f32 * out_colors;
  size_t out_count;
  
}mc_vertex_builder;

int vec3_hash(const void * _key_data, void * userdata){
  const vec3 * key_data = _key_data;
  int x = (int)round((f64)key_data->x * 10000.0);
  int y = (int)round((f64)key_data->y * 10000.0);
  int z = (int)round((f64)key_data->z * 10000.0);
  return (int)(((x * 32143217381823L + y) * 8302183104737121L + z) * 6721943213739218932L + 739213217321L);
}

void mc_take_vertex(void * userdata, vec3 pt, vec3 color){
  UNUSED(pt);
  UNUSED(color);

  mc_vertex_builder * b = userdata;
  if(b->count == b->offset){
    b->count = MAX(16, b->count * 2);
    b->verts = realloc(b->verts, b->count * sizeof(f32) * 3);
  }
  f32 * v = b->verts + b->offset * 3;
  v[0] = pt.x;
  v[1] = pt.y;
  v[2] = pt.z;
  b->offset += 1;
  //printf("?? %i\n", b->offset)
}

f32 sdf_model_sdf(void * userdata, vec3 pt, vec3 * color){
  
  sdf_model * model = userdata;
  
  return model->sdf(model->userdata, pt, color);
}
typedef struct{
  int verts[3];
  int edges[3];
}trig_face;
typedef struct{
  int v1, v2;
}face_edge;

typedef struct{
  int t1, t2;
}face_trg;

void improve_mesh(mc_vertex_builder * bld){
  hash_table * vertex_lookup;
  vec3 * vertexes = NULL;
  
  int vertex_count = 0;
  hash_table * edge_lookup;
  face_edge * edges = NULL;
  face_trg * edge_triangles = NULL;
  int edge_count = 0;

  trig_face * triangles = NULL;
  bool * skip_triangles = NULL;
  int triangle_count = 0;

  hash_table * vert_tri_lookup = lisp_malloc(sizeof(vert_tri_lookup[0]));
  ht_create3(vert_tri_lookup, 1, sizeof(int) * 2, sizeof(int));
  
  
  vertex_lookup = lisp_malloc(sizeof(*vertex_lookup));
  ht_create3(vertex_lookup, 1, sizeof(vec3), sizeof(int));
  vertex_lookup->hash = (void *)vec3_hash;
  
  edge_lookup = lisp_malloc(sizeof(*edge_lookup));
  ht_create3(edge_lookup, 1, sizeof(face_edge), sizeof(int));
  
  int id = 0;
  for(int trg = 0; trg < bld->count / 3; trg++){

    int ids[3];
    
    for(int i = 0; i < 3; i++){
      if(!ht_get(vertex_lookup, bld->verts + trg * 9 + i * 3, &id)){
        id = vertex_count;
        ht_set(vertex_lookup, bld->verts + trg * 9 + i * 3, &id);
        vertex_count += 1;
        vertexes = realloc(vertexes, vertex_count * sizeof(vertexes[0]));
        vertexes[vertex_count - 1] = ((vec3 *)&bld->verts[trg * 9 + i * 3])[0];
        
      } else{
        //printf("removed dup %i %i\n", ++dupcnt, sizeof(vec3) );
      }
      for(int j = 0; j < 32; j++){
        int id2[] = {id, j};
        int trg2 = 0;
        if(!ht_get(vert_tri_lookup, id2, &trg2)){
          ht_set(vert_tri_lookup, id2, &trg);
          break;
        }else if(trg2 == trg){
          // already added.
        }
      }
      ids[i] = id;
    }
    face_edge e1[] = {{.v1 = ids[0], .v2 = ids[1]},
                      {.v1 = ids[1], .v2 = ids[2]},
                      {.v1 = ids[2], .v2 = ids[0]}};
    int edge_ids[3];
    for(int i = 0; i < 3; i++){
      if(e1[i].v1 > e1[i].v2)
        SWAP(e1[i].v1, e1[i].v2);
      int eid;
      if(!ht_get(edge_lookup, &e1[i], &eid)){
        eid = edge_count;
        ht_set(edge_lookup, &e1[i], &eid );
        edge_count += 1;
        edges = realloc(edges, edge_count * sizeof(edges[0]));
        edges[edge_count - 1] = e1[i];
        
        edge_triangles = realloc(edge_triangles, edge_count * sizeof(edge_triangles[0]));
        edge_triangles[edge_count - 1] = (face_trg){0};
        edge_triangles[edge_count - 1].t1 = trg;
      }else{
        //printf("Reuse edge %i %i %i %i\n", eid, trg, edge_triangles[eid].t1,  edge_triangles[eid].t2);
        edge_triangles[eid].t2 = trg;
      }
      edge_ids[i] = eid;
    }
    triangle_count += 1;
    triangles = realloc(triangles, triangle_count * sizeof(triangles[0]));
    for(int i = 0; i < 3; i++){
      triangles[triangle_count - 1].verts[i] = ids[i];
      triangles[triangle_count - 1].edges[i] = edge_ids[i];
    }
  }
  var sdf = bld->sdf->sdf;
  var sdf_userdata = bld->sdf->sdf_userdata;
  vec3 c;
  
 for(int i = 0; i < vertex_count; i++){
   vec3 v = vertexes[i];
   for(int j = 0; j < 10; j++){
     var v2 = trace_point(bld->sdf, v, 0.0001);
     
     v = v2;
     if(vec3_len(vec3_sub(v,v2)) < 0.0001)
       break;
   }
   vertexes[i] = v;
 }
 if(false){
  // optimize vertex positions.
  for(int i = 0; i < vertex_count; i++){
    vec3 v= vertexes[i];
    vec3 connected[32] = {0};
    int j = 0;
    for(; j < 32; j++){
      int id2[] = {i, j};
      int trg;
      if(!ht_get(vert_tri_lookup, id2, &trg)){
        break;
      }
      var tri = triangles[trg];
      int con = -1;
      for(int k = 0; k < 3; k++){
        if(edges[tri.edges[k]].v1 == i){
          con = edges[tri.edges[k]].v2;
        }else if(edges[tri.edges[k]].v2 == i){
          con = edges[tri.edges[k]].v1;
        }
      }
      if(con != -1)
        connected[j] = vertexes[con];   
    }


    int cnt = j;
    vec3 u[] = {vec3_new(0,0,0),vec3_new(1,0,0), vec3_new(-1,0,0),
      vec3_new(0,1,0), vec3_new(0,-1,0),
      vec3_new(0,0,1), vec3_new(0,0,-1)};
    f32 ers[7];
    f32 di = 0.5;
    for(int k = 0; k < 400; k++){
      //printf("K: %i\n", k);
    //vec3_print(v), vec3_print(connected[0]);
    for(int j = 0; j < 7; j++){
      vec3 u2 = vec3_scale(u[j], di); 
      f32 e = 0;
      
      for(int i = 0; i < cnt; i++){
        vec3 mid = vec3_scale(vec3_add(connected[i], vec3_add(v, u2)), 0.5);
        var d2 = sdf(sdf_userdata, mid, &c);
        e += d2 * d2;
      }
      e = sqrtf(e);
      ers[j] = e;
    }
    //for(int i = 0; i < 7; i++)
    //  printf(" %f ", ers[i]);
    int min_i = 0;
    for(int j = 0; j < 7; j++){
      if(ers[j] < ers[min_i])
        min_i = j;
    }
    //printf("E: %f\n", ers[min_i]);
    if(min_i == 0)
      di *= 0.5;
    else
      v = vec3_add(v, vec3_scale(u[min_i], di));
    if(di < 0.0001)
      break;
    //vec3_print(vd);
    //v = vec3_sub(v, vec3_scale(vd, 1.0));
    }
    vertexes[i] = v;
      
    //printf("Count :%i \n", cnt);
  
  }
 }
 
  skip_triangles = alloc0(sizeof(skip_triangles[0]) * triangle_count);
  int new_vertexes = 0;
  int edge_count2 = 0;//edge_count;
  if(false){
  for(int i = 0; i < edge_count2; i++){
    var e = edges[i];
    var et = edge_triangles[i];
    var f1 = triangles[et.t1];
    var f2 = triangles[et.t2];
    if(skip_triangles[et.t1]) continue;
    if(skip_triangles[et.t2]) continue;
    // only edges with two triangles connected can be split
    if(et.t1 == 0 || et.t2 == 0)
      continue;

    vec3 v1 = vertexes[e.v1];
    vec3 v2 = vertexes[e.v2];
    vec3 mid = vec3_scale(vec3_add(v1, v2), 0.5);
    vec3 c;
    var d = sdf(sdf_userdata, mid, &c);
    //vec3 asd = vec3_new(0.5432, -0.3234, 0.7783);
    //vec3 cross = vec3_normalize(vec3_mul_cross(vec3_sub(v2, v1), asd));
    if(fabs(d) > 100.5){
      var m2 = mid;
      //var m2 = trace_point(bld->sdf, mid, 0.01);
      for(int i = 0; i < 0; i++)
        m2 = trace_point(bld->sdf, m2, 0.01);
      new_vertexes++;
      int id;
      if(!ht_get(vertex_lookup, &m2.x, &id)){
        id = vertex_count;
        ht_set(vertex_lookup, &m2.x , &id);
        vertex_count += 1;
        vertexes = realloc(vertexes, vertex_count * sizeof(vertexes[0]));
        vertexes[vertex_count - 1] = m2;
        
      }
      skip_triangles[et.t1] = true;
      skip_triangles[et.t2] = true;
      //one edge gets removed.
      // all other edges still exist, but are connected with the new vertex.
      if(true){
      int e1 = -1, e2 = -1, e3 = -1, e4 = -1;
      for(int j = 0; j < 3; j++){
        if(f1.edges[j] != i){
          if(e1 == -1)
            e1 = f1.edges[j];
          else
            e2 = f1.edges[j];
        }
        if(f2.edges[j] != i){
          if(e3 == -1)
            e3 = f2.edges[j];
          else
            e4 = f2.edges[j];
        }
      }
     
      // these 4 edges, along with the center vertex form 4 new triangles.
      int new_edges[] = {e1, e2, e3, e4};
      for(int j = 0; j < 4; j++){
        if(new_edges[j] == -1)
          ASSERT(false);
        //printf("new triangle\n");
        face_edge e0 = edges[new_edges[j]];
        int ids[] = {e0.v1, e0.v2, id};
        int edge_ids[3];
        face_edge e1[] = {{.v1 = ids[0], .v2 = ids[1]},
                          {.v1 = ids[1], .v2 = ids[2]},
                          {.v1 = ids[2], .v2 = ids[0]}};
        for(int i = 0; i < 3; i++){
          if(e1[i].v1 > e1[i].v2)
            SWAP(e1[i].v1, e1[i].v2);
          int eid;
          if(!ht_get(edge_lookup, &e1[i], &eid)){
            eid = edge_count;
            ht_set(edge_lookup, &e1[i], &eid );
            edge_count += 1;
            edges = realloc(edges, edge_count * sizeof(edges[0]));
            edges[edge_count - 1] = e1[i];
            
            edge_triangles = realloc(edge_triangles, edge_count * sizeof(edge_triangles[0]));
            edge_triangles[edge_count - 1] = (face_trg){0};
            edge_triangles[edge_count - 1].t1 = triangle_count;
            printf("new_edge\n");
          }else{
            printf(" edge %i %i %i %i\n", eid, triangle_count, edge_triangles[eid].t1,  edge_triangles[eid].t2);
            edge_triangles[eid].t2 = triangle_count;
          }
          edge_ids[i] = eid;
        }
        triangle_count += 1;
        triangles = realloc(triangles, triangle_count * sizeof(triangles[0]));
        skip_triangles = realloc(skip_triangles, triangle_count * sizeof(skip_triangles[0]));
        skip_triangles[triangle_count - 1] = false;
        //printf("new triangle: ");
        for(int i = 0; i < 3; i++){
          triangles[triangle_count - 1].verts[i] = ids[i];
          triangles[triangle_count - 1].edges[i] = edge_ids[i];
          //vec3_print(vertexes[ids[i]]);
        }
        //printf("\n");
      }
      //printf("EDGE: %i %i %i %i\n", e1, e2, e3, e4);
      }
    }
  }
  }

  u8 * vertex_hits = alloc0(vertex_count);;
  int skip = 0;
  for(int i = 0; i < triangle_count; i++){
    var trig = triangles[i];
    if(skip_triangles[i])
      skip += 1;
    for(int j = 0; j < 3; j++){
      vertex_hits[trig.verts[j]]++;
    }
  }
        
  
  {
    bld->out_verts = alloc0((triangle_count - skip) * 9 * sizeof(float));
    bld->out_colors = alloc0((triangle_count - skip) * 9 * sizeof(float));
    // each triangle has 3 vertes, each verts has 3 dimensions.
    f32 * vp = bld->out_verts;
    f32 * cp = bld->out_colors;
    bld->out_count = triangle_count * 3;
    for(int i = 0; i < triangle_count; i++){
      if(skip_triangles[i]){
        //printf("SKIP\n");
        continue;
      }
      var trig = triangles[i];
      for(int j = 0; j < 3; j++){
        var vx = vertexes[trig.verts[j]];
        vec3 vx_c;
        sdf(sdf_userdata, vx, &vx_c);
        //vec3_print(vx); printf("\n");
        for(int k = 0; k < 3; k++){
          vp[0] = vx.data[k];
          cp[0] = vx_c.data[k];
          vp++;
          cp++;
        }
      }
      
    }
  }


  printf("vert count: %i, edge count: %i, new_vertexes: %i\n", vertex_count, edge_count, new_vertexes);  
}

hash_table * vertex_lookup2 = NULL;
int reuse1 = 0;
int reuse2 = 0;

f32 generic_sdf2(void * ud, vec3 p, vec3 * c){
  //return generic_sdf(ud, p, c);
  if(false && vertex_lookup2 != NULL){
    f32 r[4];
    if(!ht_get(vertex_lookup2, &p.x, r)){
      vec3 c2;
      r[0] = generic_sdf(ud, p, &c2);
      r[1] = c2.x;
      r[2] = c2.z;
      r[3] = c2.y;
      ht_set(vertex_lookup2, &p.x, r);
      reuse1 += 1;
    }else{
      reuse2 += 1;
      //printf("reuse! %i %i\n", reuse1, reuse2);
    }
    if(c != NULL){
      c->x = r[1];
      c->y = r[2];
      c->z = r[3];
    }

    return r[0];
  }
  //vec3_print(p);printf("\n");
  return generic_sdf(ud, p, c);

}
void ht_empty(hash_table *ht);
lisp_value sdf_marching_cubes(lisp_value scale_v, lisp_value _res, lisp_value model0, lisp_value offset){
  if(!is_nil(model0)){
    void * model = get_physics_model_cached2(model0);
    if(model == NULL) return nil;
    //var timestamp0 = timestampf();
    //describe_sdf(model);
    
    var center = is_nil(offset) ? vec3_zero : lv_vec3(offset);
    
    var scale = lisp_value_as_rational(scale_v);
    f64 res = lisp_value_as_rational(_res);
    df_ctx ctx2 = {
      .sdf = generic_sdf2,
      .sdf_userdata = model,
      .threshold = res
     };
    
    mc_vertex_builder builder = {
      .verts = NULL,
      .colors = NULL,
      .count = 0,
      .offset = 0,
      .sdf = &ctx2
    };
    ctx2.userdata = &builder;
    ctx2.emit_point = mc_take_vertex;


    if(vertex_lookup2 != NULL){
      ht_empty(vertex_lookup2);
      dealloc(vertex_lookup2);
      vertex_lookup2 = NULL;
    }
    
    vertex_lookup2 = alloc(sizeof(*vertex_lookup2));
    ht_create3(vertex_lookup2, 1, sizeof(vec3), sizeof(f32) * 4);
    vertex_lookup2->hash = (void *)vec3_hash;
    
    //printf("Marching cubes...\n");
    //var timestamp = timestampf();
    //for(int i = 0; i < 1000; i++){
      marching_cubes_sdf(&ctx2, center, scale);
      //builder.offset = 0;
      //}
    
      //printf("Done! %f\n", (f32)(timestampf() - timestamp));
    builder.count = builder.offset;
    //timestamp = timestampf();
    improve_mesh(&builder);
    dealloc(builder.verts);
    //printf("improve mesh! %f\n", (f32)(timestampf() - timestamp));
    
    var vec3 = make_vector(integer(builder.out_count * 3), float32(0.0));
    f32 * verts2 = vec3.vector->data;
    var vec4 = make_vector(integer(builder.out_count * 3), float32(0.0));
    f32 * colors2 = vec4.vector->data;
    memcpy(verts2, builder.out_verts, sizeof(float) * 3 * builder.out_count);
    memcpy(colors2, builder.out_colors, sizeof(float) * 3 * builder.out_count);
    //println(vec3);
    //printf("POINTS: %i: %f %i\n", (int)builder.out_count, (float)(timestampf() - timestamp), (int)vertex_lookup2->count);
    //printf("finished: %f\n",  (float)(timestampf() - timestamp0));
    
    return new_cons(vec3, vec4);
  }
  vec3 center = vec3_new(0, 0.5, 0);
  f32 scale = 4.0;
  sdf_color c1 = {.type = SDF_TYPE_COLOR, .color = vec3_new(0.5, 0.9, 0.4), .model_count = 2};
  sdf_color c2 = {.type = SDF_TYPE_COLOR, .color = vec3_new(0.5, 0.4, 0.2), .model_count = 1};
  sdf_aabb a = {.type = SDF_TYPE_AABB, .pos = vec3_new(0, -10.5, 0), .size = vec3_new(100,10,100)};
  sdf_vert_capsule cap1 = {.type = SDF_TYPE_VERT_CAPSULE, .height = 3.0, .radius = 0.5, .pos = vec3_new(0,-1,0)}; 

  sdf_sphere s1 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(0.0, 3, 0), .radius = 1.5 };
  sdf_sphere s2 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(1.0, 2.0, 0), .radius = 1.0 };
  sdf_sphere s3 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(-1.0, 2.0, 0), .radius = 1.0 };
  sdf_sphere s4 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(0, -50, 0), .radius = 50};

  void * models3[] = {&s1, &s2, &s3};
  c1.models = models3;
  void * models2[] = {&cap1};
  c2.models = models2;
  void * models[] = {&c1, &c2, &s4};
  sdf_models ms = {.type = SDF_TYPE_MODELS, .model_count = 3, .models = models};
  sdf_model model = {0};
  model.sdf = generic_sdf;
  model.userdata = &a;
  model.sdf = generic_sdf;
  model.userdata = &ms;

  size_t count = 0;
  df_ctx ctx2 = {
    .sdf = sdf_model_sdf,
    .sdf_userdata = &model,
    .threshold = 0.1,
    .emit_point = mc_count_vertexes,
    .userdata = &count
  };
  
  marching_cubes_sdf(&ctx2, center, scale);

  
  var vec = make_vector(integer(count * 3 * 3), float32(0.0));
  f32 * verts = vec.vector->data;
  var vec2 = make_vector(integer(count * 3 * 3), float32(0.0));
  f32 * colors = vec2.vector->data;
  mc_vertex_builder builder = {
    .verts = verts,
    .colors = colors,
    .count = count,
    .offset = 0
  };
  ctx2.userdata = &builder;
  ctx2.emit_point = mc_take_vertex;
  
  marching_cubes_sdf(&ctx2, center, scale);
  
  printf("POINTS: %i\n", (int)count);

  return new_cons(vec, vec2);
}

lisp_value lisp_sdf_distance(lisp_value model, lisp_value p){
  if(is_nil(model))
    return nil;
  void * modelp = get_physics_model_cached2(model);
  if(modelp == NULL){
    return nil;
  }
  vec3 c;
  vec3 p2 = lv_vec3(p);
  return rational_lisp_value(generic_sdf(modelp, p2, &c));
}
void lrn(const char * l, int args, void * f);
void sdf_register(){
  lrn("sdf:dist", 2, lisp_sdf_distance);
}
typedef struct{
  int count;
}triangles_builder;

void test_marching_cubes_f(void * userdata, vec3 pt, vec3 color){
  triangles_builder * b = userdata;
  b->count += 1;
  printf("Vertex: ");
  vec3_print(pt);printf("\n");

}
void test_marching_cubes(){
  sdf_aabb a = {.pos = vec3_new(0, 0, 0), .size = vec3_new(0.5,0.5,0.5)};
  sdf_model model = {0};
  model.sdf = aabb_sdf;
  model.userdata = &a;
  process_cube(&model, vec3_new(-0.5, -0.0, -0.0), 0.2, print_triangle, NULL);

  triangles_builder b = {0};
  df_ctx ctx2 = {
    .sdf = sdf_model_sdf,
    .sdf_userdata = &model,
    .threshold = 0.001,
    .emit_point = test_marching_cubes_f,
    .userdata = &b
  };
  marching_cubes_sdf(&ctx2, vec3_new(0.1, 0.1, 0.1), 2.0);
  printf("Vertex count: %i\n", b.count);
}

void test_sdf(){
  test_sdf_col();
  printf("test layered sdf\n");
  test_layered_sdf();
  printf("Marching Cubes\n");
  //sdf_poly(nil);
  test_marching_cubes();
}

