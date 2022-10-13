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
      for(int i = 0; i < 8; i++){
        var d = candidates[i].d;
      }
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
  SDF_TYPE_AABB,
  SDF_TYPE_VERT_CAPSULE,
  SDF_TYPE_TRANSFORM,
  SDF_TYPE_MODELS,
  SDF_TYPE_COLOR
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
  mat4 inv_tform;
  void * sub_model;
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

f32 generic_sdf(void * ud, vec3 p, vec3 * c);

f32 sphere_sdf(void * ud, vec3 p, vec3 * c){
  UNUSED(c);
  sdf_sphere * a = ud;
  return vec3_len(vec3_sub(p, a->pos)) - a->radius;
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

  vec3 x1 = mat4_mul_vec3(a->inv_tform, vec3_new(0,0,0));
  vec3 x2 = mat4_mul_vec3(a->inv_tform, vec3_new(1,1,1));
  f32 eig = vec3_len(vec3_sub(x2, x1)) / sqrtf(3);
  return generic_sdf(a->sub_model, p, color) / eig;
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
      .sub_model = NULL
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
      a_t.sub_model = aabb;
    }
    if(lisp_value_eq(car(model_type), get_symbol("sphere"))){
      f32 s = lisp_value_rational(cadr(model_type));

      sdf_sphere * sphere = alloc(sizeof(*sphere));

      sphere->type = SDF_TYPE_SPHERE;
      sphere->pos = vec3_zero;
      sphere->radius = s;
      a_t.sub_model = sphere;
    }
    if(lisp_value_eq(car(model_type), get_symbol("capsule"))){
      f32 h = lisp_value_rational(cadr(model_type));
      f32 r = lisp_value_rational(caddr(model_type));

      sdf_vert_capsule * capsule = alloc(sizeof(*capsule));

      capsule->type = SDF_TYPE_VERT_CAPSULE;
      capsule->radius = r;
      capsule->height = h;
      a_t.sub_model = capsule;
    }
        
    if(a_t.sub_model != NULL){
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


void describe_sdf(void * ptr){
  static int level = 0;
  sdf_type * tp = ptr;
  switch(tp[0]){
  case SDF_TYPE_TRANSFORM:{
    sdf_transform * t = ptr;
    printf("Transform: ");
    mat4_print(t->inv_tform);
    describe_sdf(t->sub_model);
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
  case SDF_TYPE_VERT_CAPSULE:{
     sdf_vert_capsule * t = ptr;
     printf("CAPSULE r=%f h=%f\n", t->radius, t->height);
     break;
   }
  case SDF_TYPE_MODELS:{
    
    sdf_models * m = ptr;
    printf("Models: %i\n", m->model_count);
    for(size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  case SDF_TYPE_COLOR:{
    
    sdf_color * m = ptr;
    printf("COLOR: %i\n", m->model_count);
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
    sdf_lookup = lisp_make_hashtable();
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

lisp_value foxgl_detect_collision(lisp_value obj1, lisp_value obj2,lisp_value physics1, lisp_value physics2, lisp_value out_cons){
 
  var p1 = lv_vec3(car(obj1));
  var p2 = lv_vec3(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  var rot1 = lisp_value_as_rational(car(obj1));
  var rot2 = lisp_value_as_rational(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  var o1 = car(obj1);
  var o2 = car(obj2);
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
  sdf_transform a_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot1),
                                         -p1.x, -p1.y, -p1.z ),
    .sub_model = &a };
  
  void * ptr1 = get_physics_model_cached(physics1);
  if(ptr1 == NULL)
    ptr1 = &a;
  
  a_t.sub_model = ptr1;

  
  sdf_transform b_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot2),
                                         -p2.x,-p2.y,-p2.z ),
    .sub_model = &b
  };
  void * ptr2 = get_physics_model_cached(physics2);
    
  if(ptr2 == NULL)
    ptr2 = &b;
  
  b_t.sub_model = ptr2;
   
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
  var o2 = car(obj2);
  var s1l = car(floor_tile);

  void * ptr2 = get_physics_model_cached(model);
  sdf_aabb b = {.pos = p2, .size = vec3_new(0.1, 0.1, 0.1)};
  if(ptr2 == NULL)
    ptr2 = &b;

  sdf_transform b_t = {
    .type = SDF_TYPE_TRANSFORM,
    .inv_tform = mat4_translate_in_place(mat4_rotate_Y(mat4_identity(), -rot2),
                                         -p2.x, -p2.y, -p2.z ),
    .sub_model = ptr2 };

  
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
  sdf_transform t1 = {.type = SDF_TYPE_TRANSFORM, .inv_tform = t_inv, .sub_model = &s1};

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
    sdf_transform t1 = {.type = SDF_TYPE_TRANSFORM, .inv_tform = t_inv, .sub_model = &s1};
    
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
  ctx->sdf(ctx->sdf_userdata, a, &color);
  ctx->emit_point(ctx->userdata, a, color);
  
  ctx->sdf(ctx->sdf_userdata, b, &color);
  ctx->emit_point(ctx->userdata, b, color);
  
  ctx->sdf(ctx->sdf_userdata, c, &color);
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
      //size = size * 0.5;
      sdf_model model = {
        .sdf = marching_cubes_sdff,
        .userdata = ctx,
        .threshold = 0.01
      };
      process_cube(&model, position, size, marching_cubes_emit_point, ctx);
    }
    else{
      /*{
        vec3 corners[8];
        f32 o[] = {-size, size};
        f32 ds[8];
        // faces: 0 1 2 4 (z=0) 4 5 6 7 (z = 1)
        
        for(int i = 0; i < 8; i++){
          vec3 offset = vec3_new(o[i&1], o[(i>>1)&1], o[(i>>2)&1]);
          var p = vec3_add(offset, position);
          ds[i] = ctx->sdf(ctx->sdf_userdata, p, &c);
        }

        }*/

        
      

      {
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
}mc_vertex_builder;
void mc_take_vertex(void * userdata, vec3 pt, vec3 color){
  UNUSED(pt);
  UNUSED(color);

  mc_vertex_builder * b = userdata;
  f32 * v = b->verts + b->offset * 3;
  f32 * c = b->colors + b->offset * 3;
  v[0] = pt.x;
  v[1] = pt.y;
  v[2] = pt.z;
  c[0] = color.x;
  c[1] = color.y;
  c[2] = color.z;
  b->offset += 1;
}

f32 sdf_model_sdf(void * userdata, vec3 pt, vec3 * color){
  
  sdf_model * model = userdata;
  
  return model->sdf(model->userdata, pt, color);

}

lisp_value sdf_marching_cubes(lisp_value model0){
  UNUSED(model0);
  vec3 center = vec3_new(0, 0.5, 0);
  f32 scale = 4.0;
  sdf_color c1 = {.type = SDF_TYPE_COLOR, .color = vec3_new(0.5, 0.9, 0.4), .model_count = 3};
  sdf_color c2 = {.type = SDF_TYPE_COLOR, .color = vec3_new(0.5, 0.4, 0.2), .model_count = 1};
  sdf_aabb a = {.type = SDF_TYPE_AABB, .pos = vec3_new(0, 0.0, 0), .size = vec3_new(0.6,0.6,0.6)};
  sdf_aabb a2 = {.type = SDF_TYPE_AABB, .pos = vec3_new(2, 0.0, 0), .size = vec3_new(0.6,0.6,0.6)};
  sdf_vert_capsule cap1 = {.type = SDF_TYPE_VERT_CAPSULE, .height = 3.0, .radius = 0.5, .pos = vec3_new(0,-1,0)}; 

  sdf_sphere s1 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(0.0, 3, 0), .radius = 1.5 };
  sdf_sphere s2 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(1.0, 2.0, 0), .radius = 1.0 };
  sdf_sphere s3 = {.type = SDF_TYPE_SPHERE, .pos = vec3_new(-1.0, 2.0, 0), .radius = 1.0 };
  void * models3[] = {&s1, &s2, &s3};
  c1.models = models3;
  void * models2[] = {&cap1};
  c2.models = models2;
  void * models[] = {&c1, &c2};
  sdf_models ms = {.type = SDF_TYPE_MODELS, .model_count = 2, .models = models};
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
  
  printf("POINTS: %i\n", count);

  return new_cons(vec, vec2);
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

