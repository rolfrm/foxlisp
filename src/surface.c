#include <iron/full.h>
#include <iron/gl.h>
#include <microio.h>
#include "foxlisp.h"
#include "foxgl.h"
typedef enum { DITHER_NONE, DITHER_FLOYD_STEINBERG } DITHERING;

extern DITHERING dithering;

typedef struct {
  // expensive 32-bit color channels. oh well.
  f32 r, g, b;
} rgb;

typedef struct {
  f32 h, s, v;
} hsv;

hsv rgb2hsv(rgb color);
rgb hsv2rgb(hsv color);

rgb rgb_blend(rgb a, rgb b, f32 ratio);
hsv hsv_blend(hsv a, hsv b, f32 ratio);

rgb rgb_add(rgb a, rgb b);

#define RGB(r2, g2, b2)                                                        \
  { .r = r2, .g = g2, .b = b2 }
#define dot(x, y) vec2_dot(x, y)
#define dot2(x) vec2_dot(x, x)

#define VEC2(x2, y2)                                                           \
  { .x = x2, .y = y2 }
#define VEC3(x3, y3, z3)                                                       \
  { .x = x3, .y = y3, .z = z3 }

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
f32 polygon_distance(vec2 p, vec2 *v, u32 len);
// pseudo coloring for debugging distance fields.
rgb df_pseudo(f32 x);

#define MIN3(a, b, c) MIN(MIN(a, b), c)
#include "distance_fields.c"

vec2 p1 = {.x = 0, .y = 3};                     // top left coordinate.
vec2 p2 = {.x = 0, .y = 10};                    // bottom right coordinate..
rgb color1 = {.r = 0.5f, .g = 0.8f, .b = 0.5f}; // light blue
rgb color2 = {.r = 0.3f, .g = 0.5f, .b = 0.2f}; // white

rgb get_color(vec2 p) {
  // (P - P1) dot (P - P2)
  // Linearily interpolate the colors.
  f32 n = noise(p);
  vec2 lvec = vec2_sub(p2, p1);
  f32 ratio = vec2_dot(vec2_sub(p, p1), lvec) / vec2_dot(lvec, lvec);
  ratio = CLAMP(ratio, 0, 1.0f);
  return rgb_blend(color1, color2, ratio + 1 * (n - 0.5f) / 128.0f);
}

float box(vec3 p, vec3 b) {
  vec3 q = vec3_sub(vec3_abs(p), b);

  return vec3_len(vec3_max(q, vec3_zero)) + MIN(MAX(q.x, MAX(q.y, q.z)), 0.0f);
}

f32 clampf(f32 a, f32 b, f32 c) {
  if (a < b)
    return b;
  if (a > c)
    return c;
  return a;
}

float vert_capsule(vec3 p, float h, float r) {
  p.y -= clampf(p.y, 0.0, h);
  return vec3_len(p) - r;
}
f32 sphere(vec3 v, vec3 center, float radius) {
  vec3 p = center;
  return vec3_len(vec3_sub(v, p)) - radius;
}

f32 d0(vec3 v, vec3 *c, vec3 center, float radius) {
  *c = vec3_new(1, 0, 1);
  vec3 p = center;

  return vec3_len(vec3_sub(v, p)) - radius;
}

f32 d1(vec3 v, vec3 *c) {
  *c = vec3_new(0, 0, 1);
  var d1 = d0(v, c, vec3_new(0, 2, 0), 1.0);
  var d2 = d0(v, c, vec3_new(-3, 2, 0), 0.5);
  var d3 = d0(v, c, vec3_new(3, 3, 0), 0.5);
  var d4 = box(vec3_sub(v, vec3_new(0, 0.5, 0.25)), vec3_new(0.5, 0.5, 0.5));

  var d = MIN(d3, MIN(d1, MIN(d2, d4)));
  if (d == d1)
    *c = vec3_new(1, 0, 0);
  if (d == d2)
    *c = vec3_new(0, 1, 0);
  if (d == d3)
    *c = vec3_new(0, 0, 1);
  if (d == d4)
    *c = vec3_new(0, 1, 1);
  return d;
}

f32 tree(void *userdata, vec3 v, vec3 *c) {
  UNUSED(userdata);
  v = vec3_scale(v, 1.2);
  // v.y += 2;
  f32 d1 = vert_capsule(vec3_sub(v, vec3_new(0, 0, 0)), 3.0, 0.5);

  f32 d2 = sphere(v, vec3_new(0, 4, 0), 1.5);
  f32 d3 = sphere(v, vec3_new(1.3, 3.9, -0.2), 1.6);
  f32 d4 = sphere(v, vec3_new(-0.3, 3.8, -1.0), 1.4);
  f32 d5 = sphere(v, vec3_new(0.8, 4.3, 1.2), 1.4);

  var dg = MIN(d2, d3);
  dg = MIN(d4, dg);
  dg = MIN(d5, dg);
  var d = MIN(d1, dg);

  if (d == d1) {
    rgb color1 = {.r = 0.7, .g = 0.4, .b = 0.4};
    rgb color2 = {.r = 0.6, .g = 0.5, .b = 0.35};
    f32 n = noise(vec2_new(v.x, v.z));

    var col = rgb_blend(color1, color2, n);
    c->x = col.r;
    c->y = col.g;
    c->z = col.b;
  } else if (d == dg) {
    var rgb = get_color(vec2_new(v.x, v.y));
    ;
    *c = vec3_new(rgb.r, rgb.g, rgb.b);
    //*c = vec3_new(0.5,0.8,0.5);
  }
  return d;
}

typedef struct {
  f32 (*sdf)(void *userdata, vec3 v, vec3 *color);
  void (*emit_point)(void *userdata, vec3 pt, vec3 color);
  f32 threshold;
  void *userdata;
  void *sdf_userdata;
} df_ctx;

vec3 sdf_gradient(df_ctx *ctx, vec3 pt, f32 size) {
  vec3 c;
  var ptx = pt;
  var pty = pt;
  var ptz = pt;
  ptx.x += size * 0.2f;
  pty.y += size * 0.2f;
  ptz.z += size * 0.2f;
  var dx1 = ctx->sdf(ctx->sdf_userdata, ptx, &c);
  var dy1 = ctx->sdf(ctx->sdf_userdata, pty, &c);
  var dz1 = ctx->sdf(ctx->sdf_userdata, ptz, &c);

  ptx.x -= size * 0.2f * 2;
  pty.y -= size * 0.2f * 2;
  ptz.z -= size * 0.2f * 2;
  var dx2 = ctx->sdf(ctx->sdf_userdata, ptx, &c);
  var dy2 = ctx->sdf(ctx->sdf_userdata, pty, &c);
  var dz2 = ctx->sdf(ctx->sdf_userdata, ptz, &c);
  var dv = vec3_new(dx1 - dx2, dy1 - dy2, dz1 - dz2);
  var l = vec3_len(dv);
  if (fabsf(l) < 0.00001f)
    return vec3_zero;
  var x = vec3_scale(dv, 1.0f / l);
  return x;
}

vec3 trace_point(df_ctx *ctx, vec3 pt, f32 size) {
  var x = sdf_gradient(ctx, pt, size);
  vec3 c;
  var d0 = ctx->sdf(ctx->sdf_userdata, pt, &c);

  var r = vec3_sub(pt, vec3_scale(x, d0));
  return r;
}

vec3 optimize_point(df_ctx *ctx, vec3 pt, f32 size) {
  for (int i = 0; i < 10; i++) {
    var x = sdf_gradient(ctx, pt, size);
    vec3 c;
    var d0 = ctx->sdf(ctx->sdf_userdata, pt, &c);

    var r = vec3_sub(pt, vec3_scale(x, d0));
    pt = r;
    if (d0 < size * 0.1f) {
      break;
    }
  }
  return pt;
}

// idea: use the normal to reduce the number of planes.
void trace_point_cloud(df_ctx *ctx, vec3 position, f32 size) {
  f32 d;
  vec3 c;
  d = ctx->sdf(ctx->sdf_userdata, position, &c);
  if (fabsf(d) < size * 1.5f) {
    f32 s2 = size * 0.5f;

    if (size < ctx->threshold * 1.42f) {
      size = size * 0.5f;
      position = trace_point(ctx, position, size * 0.1f);
      var g = sdf_gradient(ctx, position, size * 0.1f);

      var s1 = vec3_normalize(vec3_mul_cross(g, vec3_new(g.y, g.z, g.x)));
      var s2 = vec3_mul_cross(g, s1);
      s1 = vec3_scale(s1, size);
      s2 = vec3_scale(s2, size);
      var pos2 = position;
      for (f32 j = -1; j < 2; j++) {
        for (f32 i = -1; i < 2; i++) {
          position = vec3_add(
              vec3_add(vec3_scale(s2, j * 2), vec3_scale(s1, i * 2)), pos2);
          position = trace_point(ctx, position, size * 0.1f);

          vec3 p1 = vec3_add(position, vec3_add(s1, s2));
          vec3 p2 = vec3_add(position, vec3_sub(s1, s2));
          vec3 p3 = vec3_sub(position, vec3_add(s1, s2));
          vec3 p4 = vec3_sub(position, vec3_sub(s1, s2));
          p1 = trace_point(ctx, p1, size * 0.1f);
          p2 = trace_point(ctx, p2, size * 0.1f);
          p3 = trace_point(ctx, p3, size * 0.1f);
          p4 = trace_point(ctx, p4, size * 0.1f);

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

    } else {
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

typedef struct {
  int count;
  int i;
  f32 *verts;
  f32 *colors;
} sft_context;

void emit_pt2(void *userdata, vec3 p, vec3 c) {
  sft_context *id = userdata;
  var i = id->i * 3;
  id->verts[i] = p.x;
  id->verts[i + 1] = p.y;
  id->verts[i + 2] = p.z;
  id->colors[i] = c.x;
  id->colors[i + 1] = c.y;
  id->colors[i + 2] = c.z;
  id->i += 1;
}

static void emit_pt(void *userdata, vec3 p, vec3 c) {
  UNUSED(p);
  UNUSED(c);
  sft_context *ud = userdata;
  ud->count += 1;
}
lisp_value new_stack_cons(cons *c, lisp_value a, lisp_value b) {
  c->car = a;
  c->cdr = b;
  return (lisp_value){.type = LISP_CONS, .cons = c};
}

f32 lisp_sdf_func(void *data, vec3 pt, vec3 *color) {
  lisp_value *v = data;
  var f1 = car(*v);
  lisp_vector vec;
  vec.data = pt.data;
  vec.count = 3;
  vec.elem_size = 4;
  vec.default_value.type = LISP_FLOAT32;
  lisp_value vec2 = vector_lisp_value(&vec);

  cons a, b;

  lisp_value r =
      lisp_eval(lisp_get_root_scope(),
                new_stack_cons(&a, f1, new_stack_cons(&b, vec2, nil)));
  var d = lisp_value_rational(r);

  var f2 = cdr(*v);

  lisp_value r2 =
      lisp_eval(lisp_get_root_scope(),
                new_stack_cons(&a, f2, new_stack_cons(&b, vec2, nil)));
  f32 *f = r2.vector->data;
  color->x = f[0];
  color->y = f[1];
  color->z = f[2];

  return d;
}

lisp_value sdf_poly(lisp_value f) {

  sft_context ctx = {0};

  df_ctx c = {.sdf = tree /*d1*/,
              .userdata = &ctx,
              .emit_point = emit_pt,
              .threshold = 0.25};
  if (!is_nil(f)) {
    c.sdf = lisp_sdf_func;
    c.sdf_userdata = &f;
  }
  trace_point_cloud(&c, vec3_new(0, 0, 0), 5.0);
  var vec = make_vector(integer(ctx.count * 3), float32(0.0));
  f32 *verts = vec.vector->data;
  var vec2 = make_vector(integer(ctx.count * 3), float32(0.0));
  f32 *colors = vec2.vector->data;
  ctx.colors = colors;
  ctx.verts = verts;
  c.emit_point = emit_pt2;
  trace_point_cloud(&c, vec3_new(0, 0, 0), 5.0);
  printf("POINTS: %i\n", ctx.count);
  return new_cons(vec, vec2);
}

typedef struct {
  f32 (*sdf1)(void *userdata, vec3 v, vec3 *color);
  f32 (*sdf2)(void *userdata, vec3 v, vec3 *color);
  f32 threshold;
  void *userdata1;
  void *userdata2;
  vec3 pt;
  bool collision_detected;
  f32 greatest_common_overlap;
  int iterations;
} cdf_ctx;

const float sqrt_3 = 1.73205f;

typedef int (*CompareFunction)(const void *a, const void *b, void *userdata);

void swap_elements(void *a, void *b, size_t size) {
    char *temp = malloc(size);
    memcpy(temp, a, size);
    memcpy(a, b, size);
    memcpy(b, temp, size);
    free(temp);
}

size_t partition(void *base, size_t num, size_t size, CompareFunction compare, void *userdata) {
    char *pivot = (char *)base + (num / 2) * size;
    size_t i = 0;
    size_t j = num - 1;

    while (true) {
        while (compare((char *)base + i * size, pivot, userdata) < 0) {
            i++;
        }

        while (compare((char *)base + j * size, pivot, userdata) > 0) {
            j--;
        }

        if (i >= j) {
            return j;
        }

        swap_elements((char *)base + i * size, (char *)base + j * size, size);
        i++;
        j--;
    }
}

void quick_sort(void *base, size_t num, size_t size, CompareFunction compare, void *userdata) {
    if (num <= 1) {
        return;
    }

    size_t p = partition(base, num, size, compare, userdata);
    quick_sort(base, p + 1, size, compare, userdata);
    quick_sort((char *)base + (p + 1) * size, num - p - 1, size, compare, userdata);
}

int comp_depth(const int * a, const int * b, const f32 * d){
  if(d[*a] < d[*b])
	 return -1;
  if(d[*a] > d[*b])
	 return 1;
  return 0;
}

void sdf_detect_collision(cdf_ctx *ctx, vec3 position, f32 size) {
  if (ctx->collision_detected)
    return;
  var d = ctx->sdf1(ctx->userdata1, position, NULL);
  var d2 = ctx->sdf2(ctx->userdata2, position, NULL);
  //vec3_print(position);
  //printf("%f %f %f\n", (f64)d, (f64)d2, (f64)size);
  if (d < size * sqrt_3 && d2 < size * sqrt_3) {
    f32 s2 = size * 0.5f;
	 
    if (size < ctx->threshold * sqrt_3) {
      // collison detected
      ctx->pt = position;
      ctx->collision_detected = MAX(d, d2) < -ctx->threshold;
      return;
    } else {
		f32 d1[8];
		int idx[8];
		vec3 offset[8];
		for(size_t i = 0; i < 8; i++){
		  
		  offset[i] = vec3_sub(position,
									  vec3_new(
												  -s2 * ((i&1) == 0 ? 1 : -1),
												  -s2 * (((i>>1)&1) == 0 ? 1 : -1),
												  -s2 * (((i>>2)&1) == 0 ? 1 : -1)));
		  
		  let d_a = ctx->sdf1(ctx->userdata1, offset[i], NULL);
		  let d_b = ctx->sdf2(ctx->userdata2, offset[i], NULL);
		  d1[i] = MAX(d_a, d_b);
		  idx[i] = i;
		}
		
		qsort_r(idx, 8, sizeof(int), (void *)comp_depth, d1);
		
		for(size_t i = 0; i < 8; i++){
		  size_t i2 = idx[i];
		  sdf_detect_collision(ctx, offset[i2],s2);
		  if(ctx->collision_detected)
		  	 return;
		}
    }
  }
}

typedef struct {
  f32 d, d2;
  i32 i;
} fi_pair;

int sort_pairs(const fi_pair *a, const fi_pair *b) {
  return a->d > b->d ? 1 : -1;
}

void sdf_detect_max_overlap(cdf_ctx *ctx, vec3 position, f32 size) {
  // minimize max(d(p), d2(p))
  ctx->iterations += 1;

  if (size < ctx->threshold * 1.7f) {
    var d = ctx->sdf1(ctx->userdata1, position, NULL);
    var d2 = ctx->sdf2(ctx->userdata2, position, NULL);
    var min_d = d - size * 1.73f * 0.5f;
    var min_d2 = d2 - size * 1.73f * 0.5f;

    // collison detected
    ctx->pt = position;
    ctx->collision_detected = true;
    ctx->greatest_common_overlap = MAX(min_d, min_d2);
    return;
  }

  else {
    f32 s2 = size * 0.5f;

    fi_pair candidates[8] = {0};
    f32 o[] = {-s2, s2};
    for (int i = 0; i < 8; i++) {
      vec3 offset = vec3_new(o[i & 1], o[(i >> 1) & 1], o[(i >> 2) & 1]);

      var p = vec3_add(offset, position);
      var d = ctx->sdf1(ctx->userdata1, p, NULL);
      var d2 = ctx->sdf2(ctx->userdata2, p, NULL);
      var min_d = d;
      var min_d2 = d2;
      candidates[i] =
          (fi_pair){.d = MAX(min_d2, min_d), .d2 = MIN(min_d2, min_d), .i = i};
    }
    qsort(candidates, 8, sizeof(fi_pair), (void *)sort_pairs);

    for (int _i = 0; _i < 8; _i++) {
      int i = candidates[_i].i;
      var d = candidates[i].d;
      if (d - s2 * 1.73f * 0.5f > ctx->greatest_common_overlap) {
        break;
      }

      vec3 offset = vec3_new(o[i & 1], o[(i >> 1) & 1], o[(i >> 2) & 1]);
      var p = vec3_add(offset, position);
      sdf_detect_max_overlap(ctx, p, s2);
    }
  }
}

static vec3 lv_vec3(lisp_value v) {
  return vec3_new(lisp_value_as_rational(car(v)),
                  lisp_value_as_rational(cadr(v)),
                  lisp_value_as_rational(caddr(v)));
}

// static lisp_value vec3_lv(vec3 v){
//   return
//   new_cons(rational_lisp_value(v.x),new_cons(rational_lisp_value(v.y),new_cons(rational_lisp_value(v.z),
//   nil)));
// }
static mat4 lisp_value_mat4(lisp_value v) {
  mat4 *m = lisp_value_vector(v)->data;
  return *m;
}

typedef enum {
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
} sdf_type;

typedef struct {
  sdf_type type;
  vec3 pos;
  vec3 size;
} sdf_aabb;

typedef struct {
  sdf_type type;
  vec3 pos;
  f32 radius;
} sdf_sphere;

typedef struct {
  sdf_type type;
  f32 radius;
  f32 height;
  vec3 pos;

} sdf_vert_capsule;
// f32 d1 = vert_capsule(vec3_sub(v, vec3_new(0,0,0)), 3.0, 0.5);

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
  mat4 inv_tform;
} sdf_transform;

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
} sdf_models;

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
  vec3 color;

} sdf_color;

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
  f32 soft;

} sdf_soft;

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
  vec3 pos;
  f32 radius;

} sdf_sphere_bounds;

typedef struct {
  sdf_type type;
  void **models;
  size_t model_count;
  f32 k;

} sdf_subtract;

typedef struct {
  int seed;
  int octaves;
  vec3 offset;
} sdf_noisefield;

f32 generic_sdf(void *ud, vec3 p, vec3 *c);
f32 models_sdf(void *ud, vec3 p, vec3 *color);

f32 sphere_sdf(void *ud, vec3 p, vec3 *c) {
  UNUSED(c);
  sdf_sphere *a = ud;
  return vec3_len(vec3_sub(p, a->pos)) - a->radius;
}

f32 sphere_bounds_sdf(void *ud, vec3 p, vec3 *c) {
  UNUSED(c);
  sdf_sphere_bounds *a = ud;
  var d = vec3_len(vec3_sub(p, a->pos)) - a->radius * 1.5f;
  if (d < 0.0f) {

    return models_sdf(ud, p, c);
  }
  // return models_sdf(ud, p, c);
  return d + a->radius * 0.5f;
}

f32 aabb_sdf(void *ud, vec3 p, vec3 *color) {
  UNUSED(color);
  sdf_aabb *a = ud;

  p = vec3_sub(p, a->pos);
  vec3 q = vec3_sub(vec3_abs(p), a->size);
  return vec3_len(vec3_max(q, vec3_zero)) + MIN(MAX(q.x, MAX(q.y, q.z)), 0.0f);
}

f32 vert_capsule_sdf(void *ud, vec3 p, vec3 *c) {
  UNUSED(c);
  sdf_vert_capsule *a = ud;
  return vert_capsule(vec3_sub(p, a->pos), a->height, a->radius);
}

f32 transform_sdf(void *ud, vec3 p, vec3 *color) {
  sdf_transform *a = ud;
  p = mat4_mul_vec3(a->inv_tform, p);

  // vec3 x1 = mat4_mul_vec3(a->inv_tform, vec3_new(0,0,0));
  // vec3 x2 = mat4_mul_vec3(a->inv_tform, vec3_new(1,1,1));
  // f32 eig = vec3_len(vec3_sub(x2, x1)) / sqrtf(3);
  return models_sdf(ud, p, color); // / eig;
}

f32 models_sdf(void *ud, vec3 p, vec3 *color) {
  sdf_models *models = ud;
  f32 d = 100000;
  vec3 c2 = color != NULL ? *color : vec3_zero;
  vec3 *c2p = color != NULL ? &c2 : NULL;
  for (size_t i = 0; i < models->model_count; i++) {
    var d1 = generic_sdf(models->models[i], p, c2p);
    if (d1 < d) {
      d = d1;
      if (color != NULL)
        *color = c2;
    }
  }
  return d;
}
f32 color_sdf(void *ud, vec3 p, vec3 *color) {
  sdf_color *color_elem = ud;
  if (color != NULL) {
    *color = color_elem->color;
  }
  return models_sdf(ud, p, color);
}

f32 soft_sdf(void *ud, vec3 p, vec3 *color) {
  sdf_soft *soft = ud;

  return models_sdf(ud, p, color) - soft->soft;
}
// x×(1−a)+y×a.
f32 mixf(f32 x, f32 y, f32 a) { return x * (1.f - a) + y * a; }
// float opSmoothSubtraction( float d1, float d2, float k ) {
//     float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
//     return mix( d2, -d1, h ) + k*h*(1.0-h); }

f32 subtract_sdf(void *ud, vec3 p, vec3 *color) {
  sdf_subtract *soft = ud;
  var k = soft->k;
  if (soft->model_count == 0)
    return 1000000.0f;
  var d1 = generic_sdf(soft->models[0], p, color);
  for (size_t i = 1; i < soft->model_count; i++) {
    var d2 = generic_sdf(soft->models[i], p, NULL);
    float h = CLAMP(0.5f - 0.5f * (d2 + d1) / k, 0.0f, 1.0f);
    d1 = mixf(d1, -d2, h) + k * h * (1.0f - h);
  }

  return d1;
}

vec3 *color_out = NULL;
f32 generic_sdf(void *ud, vec3 p, vec3 *c) {
  sdf_type *tp = ud;
  switch (tp[0]) {
  case SDF_TYPE_TRANSFORM:
    return transform_sdf(ud, p, c);
  case SDF_TYPE_AABB:
    return aabb_sdf(ud, p, c);
  case SDF_TYPE_SPHERE:
    return sphere_sdf(ud, p, c);
  case SDF_TYPE_VERT_CAPSULE:
    return vert_capsule_sdf(ud, p, c);
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
static void *get_physics_sdf(lisp_value value) {
  println(value);
  void **models = NULL;
  size_t model_count = 0;
  while (is_cons(value)) {
    var tform = caar(value);
    mat4 *m = lisp_value_vector(tform)->data;

    sdf_transform a_t = {.type = SDF_TYPE_TRANSFORM,
                         .inv_tform = mat4_invert(*m),
                         .models = alloc0(sizeof(void *)),
                         .model_count = 0};

    var model_type = cdar(value);
    println(model_type);
    if (lisp_value_eq(car(model_type), get_symbol("aabb"))) {
      vec3 s = lv_vec3(cdr(model_type));

      sdf_aabb *aabb = alloc(sizeof(*aabb));
      aabb->type = SDF_TYPE_AABB;
      aabb->pos = vec3_zero;
      aabb->size = s;
      vec3_print(s);
      printf("<--\n");
      a_t.models[0] = aabb;
      a_t.model_count = 1;
    }
    if (lisp_value_eq(car(model_type), get_symbol("sphere"))) {
      f32 s = lisp_value_rational(cadr(model_type));

      sdf_sphere *sphere = alloc(sizeof(*sphere));

      sphere->type = SDF_TYPE_SPHERE;
      sphere->pos = vec3_zero;
      sphere->radius = s;
      a_t.models[0] = sphere;
      a_t.model_count = 1;
    }
    if (lisp_value_eq(car(model_type), get_symbol("capsule"))) {
      f32 h = lisp_value_rational(cadr(model_type));
      f32 r = lisp_value_rational(caddr(model_type));

      sdf_vert_capsule *capsule = alloc(sizeof(*capsule));

      capsule->type = SDF_TYPE_VERT_CAPSULE;
      capsule->radius = r;
      capsule->height = h;
      a_t.models[0] = capsule;
      a_t.model_count = 1;
    }

    if (a_t.model_count > 0) {
      models = realloc(models, sizeof(void *) * ++model_count);
      models[model_count - 1] = iron_clone(&a_t, sizeof(a_t));
    }
    value = cdr(value);
  }
  if (models == NULL)
    return NULL;
  if (model_count == 1) {
    var r = models[0];
    dealloc(models);
    return r;
  }
  return models;
}
vec3 color_from_integer(int color) {
  u8 *color2 = (u8 *)&color;
  return vec3_scale(vec3_new(color2[0], color2[1], color2[2]), 1.0 / 255);
}
static void *get_physics_sdf2(lisp_value value) {
  var model_type = car(value);

  if (lisp_value_eq(model_type, get_symbol("transform"))) {

    sdf_transform *transform = alloc(sizeof(*transform));
    transform->type = SDF_TYPE_TRANSFORM;
    mat4 *m = lisp_value_vector(cadr(value))->data;
    transform->inv_tform = mat4_invert(*m);
    value = cddr(value);
    transform->model_count = lisp_length(value).integer;
    transform->models = alloc0(sizeof(void *) * transform->model_count);
    for (size_t i = 0; i < transform->model_count; i++) {
      transform->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return transform;
  }

  if (lisp_value_eq(model_type, get_symbol("offset"))) {
    value = cdr(value);
    sdf_transform *transform = alloc(sizeof(*transform));
    transform->type = SDF_TYPE_TRANSFORM;
    vec3 s = lv_vec3(car(value));

    mat4 m = mat4_translate(-s.x, -s.y, -s.z);
    ;

    transform->inv_tform = m;

    value = cdr(value);
    transform->model_count = lisp_length(value).integer;
    transform->models = alloc0(sizeof(void *) * transform->model_count);
    for (size_t i = 0; i < transform->model_count; i++) {
      transform->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return transform;
  }

  if (lisp_value_eq(model_type, get_symbol("rgb"))) {

    sdf_color *rgb = alloc(sizeof(*rgb));
    rgb->type = SDF_TYPE_COLOR;
    if (is_integer(cadr(value))) {
      rgb->color = color_from_integer(cadr(value).integer);
    } else {
      rgb->color = lv_vec3(cadr(value));
    }
    value = cddr(value);

    rgb->model_count = lisp_length(value).integer;
    rgb->models = alloc0(sizeof(void *) * rgb->model_count);
    for (size_t i = 0; i < rgb->model_count; i++) {
      rgb->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return rgb;
  }

  if (lisp_value_eq(model_type, get_symbol("soft"))) {

    sdf_soft *soft = alloc(sizeof(*soft));
    soft->type = SDF_TYPE_SOFT;
    soft->soft = lisp_value_as_rational(cadr(value));
    value = cddr(value);

    soft->model_count = lisp_length(value).integer;
    soft->models = alloc0(sizeof(void *) * soft->model_count);
    for (size_t i = 0; i < soft->model_count; i++) {
      soft->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return soft;
  }

  if (lisp_value_eq(model_type, get_symbol("subtract"))) {

    sdf_subtract *subtract = alloc(sizeof(*subtract));
    subtract->type = SDF_TYPE_SUBTRACT;
    subtract->k = lisp_value_as_rational(cadr(value));
    value = cddr(value);

    subtract->model_count = lisp_length(value).integer;
    subtract->models = alloc0(sizeof(void *) * subtract->model_count);
    for (size_t i = 0; i < subtract->model_count; i++) {
      subtract->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }
    return subtract;
  }
  if (lisp_value_eq(model_type, get_symbol("aabb"))) {
    vec3 s = lv_vec3(cdr(value));

    sdf_aabb *aabb = alloc(sizeof(*aabb));
    aabb->type = SDF_TYPE_AABB;
    aabb->pos = vec3_zero;
    aabb->size = s;
    return aabb;
  }
  if (lisp_value_eq(model_type, get_symbol("sphere"))) {
    f32 s = lisp_value_rational(cadr(value));

    sdf_sphere *sphere = alloc(sizeof(*sphere));

    sphere->type = SDF_TYPE_SPHERE;
    sphere->pos = vec3_zero;
    sphere->radius = s;
    return sphere;
  }

  if (lisp_value_eq(model_type, get_symbol("sphere-bounds"))) {

    sdf_sphere_bounds *sphere = alloc(sizeof(*sphere));

    sphere->type = SDF_TYPE_SPHERE_BOUNDS;
    sphere->pos = vec3_zero;
    sphere->radius = 0.0;

    value = cdr(value);
    sphere->model_count = lisp_length(value).integer;
    sphere->models = alloc0(sizeof(void *) * sphere->model_count);
    for (size_t i = 0; i < sphere->model_count; i++) {
      sphere->models[i] = get_physics_sdf2(car(value));
      value = cdr(value);
    }

    vec3 check_points[] = {
        vec3_new(1, 1, 1),   vec3_new(-1, 1, 1),  vec3_new(1, -1, 1),
        vec3_new(-1, -1, 1), vec3_new(1, 1, -1),  vec3_new(-1, 1, 1),
        vec3_new(1, -1, -1), vec3_new(-1, -1, -1)

    };
    f32 d[8];
    vec3 c;
    vec3 center = vec3_zero;
    for (int i = 0; i < 8; i++)
      check_points[i] = vec3_scale(check_points[i], 5000);

    for (int i = 0; i < 8; i++) {
      vec3 dir = vec3_normalize(vec3_sub(center, check_points[i]));
      f32 maxd = 100000.0;
      for (size_t j = 0; j < sphere->model_count; j++) {
        f32 d2 = generic_sdf(sphere->models[j], check_points[i], &c);
        if (d2 < maxd)
          maxd = d2;
      }
      d[i] = maxd;
      check_points[i] = vec3_add(check_points[i], vec3_scale(dir, d[i]));
      center = vec3_add(center, vec3_scale(check_points[i], 1.0f / 8.0f));
    }
    sphere->pos = center;
    sphere->radius = 0.0;
    for (int i = 0; i < 8; i++)
      sphere->radius =
          fmaxf(sphere->radius, vec3_len(vec3_sub(check_points[i], center)));
    return sphere;
  }

  if (lisp_value_eq(model_type, get_symbol("capsule"))) {
    f32 h = lisp_value_rational(cadr(model_type));
    f32 r = lisp_value_rational(caddr(model_type));

    sdf_vert_capsule *capsule = alloc(sizeof(*capsule));

    capsule->type = SDF_TYPE_VERT_CAPSULE;
    capsule->radius = r;
    capsule->height = h;
    return capsule;
  }
  println(value);
  if (is_symbol(model_type)) {
    println(model_type);
    printf("UNKNOWN MODEL!!\n");
    lisp_error(new_cons(get_symbol("Unknown symbol"), model_type));
    return NULL;
  }

  size_t count = 0;
  var value2 = value;
  lisp_value first = nil;
  while (!is_nil(value2)) {
    if (is_cons(car(value2))) {
      count += 1;
      first = car(value2);
    }

    value2 = cdr(value2);
  }

  printf("iter sub: %i\n", (int)count);
  if (count == 0)
    return NULL;
  if (count == 1)
    return get_physics_sdf2(first);
  sdf_models *mod = alloc0(sizeof(*mod));
  mod->type = SDF_TYPE_MODELS;
  mod->model_count = count;
  mod->models = alloc0(sizeof(void *) * mod->model_count);
  value2 = value;
  int i = 0;
  while (!is_nil(value2)) {
    if (is_cons(car(value))) {
      mod->models[i] = get_physics_sdf2(car(value));
      i++;
    }
    value = cdr(value);
  }
  return mod;
}

void describe_sdf(void *ptr) {
  sdf_type *tp = ptr;
  switch (tp[0]) {
  case SDF_TYPE_TRANSFORM: {
    sdf_transform *t = ptr;
    printf("Transform: ");
    mat4_print(t->inv_tform);
    for (size_t i = 0; i < t->model_count; i++)
      describe_sdf(t->models[i]);
    break;
  }
  case SDF_TYPE_AABB: {
    sdf_aabb *t = ptr;
    printf("AABB ");
    vec3_print(t->pos);
    vec3_print(t->size);
    printf("\n");
    break;
  }
  case SDF_TYPE_SPHERE: {
    sdf_sphere *t = ptr;
    printf("SPHERE %f\n", (f64)t->radius);
    break;
  }
  case SDF_TYPE_SPHERE_BOUNDS: {
    sdf_sphere_bounds *t = ptr;
    printf("SPHERE Bounds %f  ", (f64)t->radius);
    vec3_print(t->pos);
    printf("\n");
    break;
  }
  case SDF_TYPE_VERT_CAPSULE: {
    sdf_vert_capsule *t = ptr;
    printf("CAPSULE r=%f h=%f\n", (f64)t->radius, (f64)t->height);
    break;
  }
  case SDF_TYPE_MODELS: {

    sdf_models *m = ptr;
    printf("Models: %i\n", (i32)m->model_count);
    for (size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  case SDF_TYPE_COLOR: {

    sdf_color *m = ptr;
    printf("COLOR: %i\n", (i32)m->model_count);
    for (size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  case SDF_TYPE_SOFT: {

    sdf_soft *m = ptr;
    printf("SOFT: %f\n", (f64)m->soft);
    for (size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  case SDF_TYPE_SUBTRACT: {

    sdf_subtract *m = ptr;
    printf("SUBTRACT: %f\n", (f64)m->k);
    for (size_t i = 0; i < m->model_count; i++)
      describe_sdf(m->models[i]);
    break;
  }
  default:
    printf("EEERR\n");
  }
}
void *get_physics_model_cached(lisp_value physics1) {
  if (is_nil(sdf_lookup)) {
    sdf_lookup = lisp_make_hashtable(NULL, 0);
    lisp_register_value("++sdf-lookup", sdf_lookup);
  }
  if (!is_nil(physics1)) {
    var thing = lisp_hashtable_get(sdf_lookup, physics1);
    if (is_nil(thing)) {
      void *sdf = get_physics_sdf(physics1);
      if (sdf == NULL)
        thing = integer_lisp_value(0);
      else
        thing = native_pointer_lisp_value(sdf);
      lisp_hashtable_set(sdf_lookup, physics1, thing);
    }
    return thing.pointer;
  }
  return NULL;
}
void *get_physics_model_cached2(lisp_value physics1) {
  if (is_nil(sdf_lookup)) {
    sdf_lookup = lisp_make_hashtable(NULL, 0);
    lisp_register_value("++sdf-lookup", sdf_lookup);
  }
  if (!is_nil(physics1)) {
    var thing = lisp_hashtable_get(sdf_lookup, physics1);
    if (is_nil(thing)) {
      void *sdf = get_physics_sdf2(physics1);
      printf("Physics model: %p\n", sdf);
      if (sdf == NULL)
        thing = integer_lisp_value(0);
      else
        thing = native_pointer_lisp_value(sdf);
      lisp_hashtable_set(sdf_lookup, physics1, thing);
    }
    return thing.pointer;
  }
  return NULL;
}

lisp_value foxgl_detect_collision(lisp_value obj1, lisp_value obj2,
                                  lisp_value physics1, lisp_value physics2,
                                  lisp_value out_cons) {

  var p1 = lv_vec3(car(obj1));
  var p2 = lv_vec3(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  var rot1 = lisp_value_as_rational(car(obj1));
  var rot2 = lisp_value_as_rational(car(obj2));
  obj1 = cdr(obj1);
  obj2 = cdr(obj2);
  sdf_aabb a = {.type = SDF_TYPE_AABB,
                .pos = vec3_zero,
                .size = vec3_new(0.25, 05, 0.25)};
  sdf_aabb b = {.type = SDF_TYPE_AABB,
                .pos = vec3_zero,
                .size = vec3_new(0.25, 0.5, 0.25)};
  void *m1[] = {&a};
  sdf_transform a_t = {
      .type = SDF_TYPE_TRANSFORM,
      .inv_tform = mat4_translate_in_place(
          mat4_rotate_Y(mat4_identity(), -rot1), -p1.x, -p1.y, -p1.z),
      .models = m1,
      .model_count = 1};

  void *ptr1 = get_physics_model_cached(physics1);
  if (ptr1 == NULL)
    ptr1 = &a;

  m1[0] = ptr1;

  void *m2[] = {&b};

  sdf_transform b_t = {
      .type = SDF_TYPE_TRANSFORM,
      .inv_tform = mat4_translate_in_place(
          mat4_rotate_Y(mat4_identity(), -rot2), -p2.x, -p2.y, -p2.z),
      .models = m2,
      .model_count = 1};
  void *ptr2 = get_physics_model_cached(physics2);

  if (ptr2 == NULL)
    ptr2 = &b;
  m2[0] = ptr2;

  cdf_ctx ctx = {.sdf1 = transform_sdf,
                 .sdf2 = transform_sdf,
                 .userdata1 = &a_t,
                 .userdata2 = &b_t,
                 .threshold = 0.01,
                 .greatest_common_overlap = 100.0};
  cdf_ctx ctx2 = ctx;
  sdf_detect_collision(&ctx, p2, 10.0);
  if (ctx.collision_detected && is_cons(out_cons)) {
    ctx2.threshold = 0.01;

    describe_sdf(ctx.userdata1);
    describe_sdf(ctx.userdata2);
    sdf_detect_max_overlap(&ctx2, p2, 10.0);
    set_car(out_cons, rational_lisp_value((f64)ctx2.pt.x));
    set_cdr(out_cons, rational_lisp_value((f64)ctx2.pt.z));
  }
  return ctx.collision_detected ? t : nil;
}

lisp_value foxgl_detect_collision2(lisp_value obj1, lisp_value obj2,
                                   lisp_value tform2, lisp_value out_cons) {

  void *model1 = get_physics_model_cached2(obj1);
  void *model2 = get_physics_model_cached2(obj2);
  mat4 tform = lisp_value_mat4(tform2);
  vec3 p2 = vec3_new(0.5, 0.5, 0.5);
  p2 = mat4_mul_vec3(tform, p2);

  sdf_transform a_t = {.type = SDF_TYPE_TRANSFORM,
                       .inv_tform = tform,
                       .models = &model2,
                       .model_count = 1};

  cdf_ctx ctx = {.sdf1 = generic_sdf,
                 .sdf2 = generic_sdf,
                 .userdata1 = model1,
                 .userdata2 = &a_t,
                 .threshold = 0.1,
                 .greatest_common_overlap = 100.0};
  cdf_ctx ctx2 = ctx;

  sdf_detect_collision(&ctx, p2, 100.0);
  if (ctx.collision_detected && is_cons(out_cons)) {
    ctx2.threshold = 0.001;
	 
    sdf_detect_max_overlap(&ctx2, p2, 100.0);
	 vec3 p3 = ctx2.pt;
	 var itform = mat4_invert(tform);
	 p3 = mat4_mul_vec3(itform, p3);
	 //var p0 = mat4_mul_vec3(itform, vec3_new(0,0,0));
	 //p3 = vec3_sub(p3, p0);
	 set_car(out_cons, rational_lisp_value((f64)p3.x));
    set_cdr(out_cons, rational_lisp_value((f64)p3.z));
  }
  return ctx.collision_detected ? t : nil;
}

lisp_value foxgl_detect_collision3(lisp_value obj1, lisp_value obj2,
											  lisp_value l_tform1, lisp_value l_tform2, lisp_value out_cons) {

  void *model1 = get_physics_model_cached2(obj1);
  void *model2 = get_physics_model_cached2(obj2);
  mat4 tform1 = lisp_value_mat4(l_tform1);
  mat4 tform2 = lisp_value_mat4(l_tform2);
  vec3 p2 = vec3_new(0.0, 0.0, 0.0);
  p2 = mat4_mul_vec3(tform1, p2);
  vec3 p2_2 = vec3_new(0.0, 0.0, 0.0);
  p2_2 = mat4_mul_vec3(tform2, vec3_new(0.0, 0.0, 0.0));
  p2 = vec3_scale(vec3_add(p2, p2_2), 0.5);
  
  sdf_transform a_t = {.type = SDF_TYPE_TRANSFORM,
                       .inv_tform = tform1,
                       .models = &model1,
                       .model_count = 1};

  sdf_transform b_t = {.type = SDF_TYPE_TRANSFORM,
                       .inv_tform = tform2,
                       .models = &model2,
                       .model_count = 1};
  
  cdf_ctx ctx = {.sdf1 = generic_sdf,
                 .sdf2 = generic_sdf,
                 .userdata1 = &b_t,
                 .userdata2 = &a_t,
                 .threshold = 0.1,
                 .greatest_common_overlap = 100.0};
  cdf_ctx ctx2 = ctx;

  sdf_detect_collision(&ctx, p2, 100.0);
  if (ctx.collision_detected && is_vector(out_cons)) {
    ctx2.threshold = 0.001;
	 
    sdf_detect_max_overlap(&ctx2, p2, 100.0);
	 vec3 p3 = ctx2.pt;
	 vector_set(out_cons, integer(0), rational_lisp_value((f64)-p3.x));
	 vector_set(out_cons, integer(1), rational_lisp_value((f64)-p3.y));
	 vector_set(out_cons, integer(2), rational_lisp_value((f64)-p3.z));

	 var d1 = ctx.sdf1(ctx.userdata1, p3, NULL);
	 var d2 = ctx.sdf2(ctx.userdata2, p3, NULL);
	 

	 vector_set(out_cons, integer(3), rational_lisp_value((f64)d1));
	 vector_set(out_cons, integer(4), rational_lisp_value((f64)d2));
	 
	 
  }
  return ctx.collision_detected ? t : nil;
}


lisp_value foxgl_detect_collision_floor(lisp_value floor_tile, lisp_value obj2,
                                        lisp_value model) {
  var p1l = car(floor_tile);
  var p1 = vec3_new(lisp_value_as_rational(car(p1l)), 0,
                    lisp_value_as_rational(cadr(p1l)));
  var p2 = lv_vec3(car(obj2));
  floor_tile = cdr(floor_tile);
  obj2 = cdr(obj2);
  var rot2 = lisp_value_as_rational(car(obj2));
  obj2 = cdr(obj2);
  var s1l = car(floor_tile);

  void *ptr2 = get_physics_model_cached(model);
  sdf_aabb b = {.pos = p2, .size = vec3_new(0.1, 0.1, 0.1)};
  if (ptr2 == NULL)
    ptr2 = &b;
  void *m1[] = {ptr2};
  sdf_transform b_t = {
      .type = SDF_TYPE_TRANSFORM,
      .inv_tform = mat4_translate_in_place(
          mat4_rotate_Y(mat4_identity(), -rot2), -p2.x, -p2.y, -p2.z),
      .models = m1,
      .model_count = 1};

  var size = vec3_new(lisp_value_as_rational(car(s1l)), 10.0,
                      lisp_value_as_rational(cadr(s1l)));
  sdf_aabb a = {.type = SDF_TYPE_AABB,
                .pos = p1,
                .size = vec3_sub(size, vec3_new(0.5, 0.5, 0.5))};
  cdf_ctx ctx = {.sdf1 = aabb_sdf,
                 .sdf2 = transform_sdf,
                 .userdata1 = &a,
                 .userdata2 = &b_t,
                 .threshold = 0.01,
                 .greatest_common_overlap = 100.0};

  sdf_detect_collision(&ctx, p2, 10.0);

  return ctx.collision_detected ? t : nil;
}

void test_sdf_col() {

  sdf_sphere s1 = {.pos = vec3_new(-0.1f, -0.1f, 0), .radius = 1.0f};
  sdf_sphere s2 = {.pos = vec3_new(1.599f, 0, 0), .radius = 1.0f};
  cdf_ctx ctx = {.sdf1 = sphere_sdf,
                 .sdf2 = sphere_sdf,
                 .userdata1 = &s1,
                 .userdata2 = &s2,
                 .threshold = 0.01f,
                 .greatest_common_overlap = 10.0f};
  sdf_detect_max_overlap(&ctx, vec3_new(2, 2, 2), 10.0f);
  var truth = vec3_len(vec3_sub(s1.pos, s2.pos)) - s1.radius - s2.radius;

  printf("MAx overlap: %f == %f %i? \n", (f64)ctx.greatest_common_overlap,
         (f64)truth, ctx.iterations);
  if (ctx.greatest_common_overlap > 0.0f) {
    return;
  }
  // return;

  {
    sdf_aabb a = {.pos = vec3_new(0, 0, 0), .size = vec3_new(0.5, 0.5, 0.5)};
    sdf_aabb b = {.pos = vec3_new(0.9, 0, 0.), .size = vec3_new(0.5, 0.5, 0.5)};
    cdf_ctx ctx = {.sdf1 = aabb_sdf,
                   .sdf2 = aabb_sdf,
                   .userdata1 = &a,
                   .userdata2 = &b,
                   .threshold = 0.01,
                   .greatest_common_overlap = 10};
    sdf_detect_max_overlap(&ctx, vec3_new(0, 0, 0), 10.0);
    var truth = vec3_len(vec3_sub(s1.pos, s2.pos)) - s1.radius - s2.radius;

    printf("MAx overlap: %f == %f %i? \n", (f64)ctx.greatest_common_overlap,
           (f64)truth, ctx.iterations);
  }
}

void test_layered_sdf() {}

#include "mc.h"

void print_triangle(void *ud, vec3 v1, vec3 v2, vec3 v3) {
  UNUSED(ud);
  UNUSED(v1);
  UNUSED(v2);
  UNUSED(v3);
}

void marching_cubes_emit_point(void *userdata, vec3 a, vec3 b, vec3 c) {
  df_ctx *ctx = userdata;
  vec3 color = vec3_new(0, 0, 1);
  ctx->emit_point(ctx->userdata, a, color);
  ctx->emit_point(ctx->userdata, b, color);
  ctx->emit_point(ctx->userdata, c, color);
}

f32 marching_cubes_sdff(void *userdata, vec3 pt, vec3 *color) {
  df_ctx *ctx = userdata;
  return ctx->sdf(ctx->sdf_userdata, pt, color);
}

void marching_cubes_sdf(df_ctx *ctx, vec3 position, f32 size) {
  f32 d;
  vec3 c;
  d = ctx->sdf(ctx->sdf_userdata, position, &c);
  if (d < size * sqrt_3) {
    if (size <= ctx->threshold) {
      sdf_model model = {
          .sdf = marching_cubes_sdff, .userdata = ctx, .threshold = 0.01f};
      process_cube(&model, position, size, marching_cubes_emit_point, ctx);
    } else {

      f32 s2 = size * 0.5f;
      f32 o[] = {-s2, s2};
      for (int i = 0; i < 8; i++) {
        vec3 offset = vec3_new(o[i & 1], o[(i >> 1) & 1], o[(i >> 2) & 1]);
        var p = vec3_add(offset, position);
        marching_cubes_sdf(ctx, p, s2);
      }
    }
  }
}

void mc_count_vertexes(void *userdata, vec3 pt, vec3 color) {
  UNUSED(pt);
  UNUSED(color);
  size_t *c = userdata;
  c[0] += 1;
}

typedef struct {
  f32 *verts;
  f32 *colors;
  size_t count;
  size_t offset;
  int dupcnt;
  df_ctx *sdf;

  f32 *out_verts;
  f32 *out_colors;
  size_t out_count;

} mc_vertex_builder;

int vec3_hash(const void *_key_data, void *userdata) {
  UNUSED(userdata);
  const vec3 *key_data = _key_data;
  int x = (int)round((f64)key_data->x * 10000.0);
  int y = (int)round((f64)key_data->y * 10000.0);
  int z = (int)round((f64)key_data->z * 10000.0);
  return (int)(((x * 32143217381823L + y) * 8302183104737121L + z) *
                   6721943213739218932L +
               739213217321L);
}

void mc_take_vertex(void *userdata, vec3 pt, vec3 color) {
  UNUSED(pt);
  UNUSED(color);

  mc_vertex_builder *b = userdata;
  if (b->count == b->offset) {
    b->count = MAX((size_t)16, b->count * 2);
    b->verts = realloc(b->verts, b->count * sizeof(f32) * 3);
  }
  f32 *v = b->verts + b->offset * 3;
  v[0] = pt.x;
  v[1] = pt.y;
  v[2] = pt.z;
  b->offset += 1;
  // printf("?? %i\n", b->offset)
}

f32 sdf_model_sdf(void *userdata, vec3 pt, vec3 *color) {

  sdf_model *model = userdata;

  return model->sdf(model->userdata, pt, color);
}
typedef struct {
  int verts[3];
  int edges[3];
} trig_face;
typedef struct {
  int v1, v2;
} face_edge;

typedef struct {
  int t1, t2;
} face_trg;

void improve_mesh(mc_vertex_builder *bld) {
  hash_table *vertex_lookup;
  vec3 *vertexes = NULL;

  int vertex_count = 0;
  hash_table *edge_lookup;
  face_edge *edges = NULL;
  face_trg *edge_triangles = NULL;
  int edge_count = 0;

  trig_face *triangles = NULL;
  bool *skip_triangles = NULL;
  int triangle_count = 0;

  hash_table *vert_tri_lookup = lisp_malloc(sizeof(vert_tri_lookup[0]));
  ht_create3(vert_tri_lookup, 1, sizeof(int) * 2, sizeof(int));

  vertex_lookup = lisp_malloc(sizeof(*vertex_lookup));
  ht_create3(vertex_lookup, 1, sizeof(vec3), sizeof(int));
  vertex_lookup->hash = (void *)vec3_hash;

  edge_lookup = lisp_malloc(sizeof(*edge_lookup));
  ht_create3(edge_lookup, 1, sizeof(face_edge), sizeof(int));
  printf("bld->count %i\n", bld->count);
  int id = 0;
  for (size_t trg = 0; trg < bld->count / 3; trg++) {

    int ids[3];

    for (int i = 0; i < 3; i++) {
      if (!ht_get(vertex_lookup, bld->verts + trg * 9 + i * 3, &id)) {
        id = vertex_count;
        ht_set(vertex_lookup, bld->verts + trg * 9 + i * 3, &id);
        vertex_count += 1;
        vertexes = realloc(vertexes, vertex_count * sizeof(vertexes[0]));
        vertexes[vertex_count - 1] = ((vec3 *)&bld->verts[trg * 9 + i * 3])[0];

      } else {
        // printf("removed dup %i %i\n", ++dupcnt, sizeof(vec3) );
      }
      for (int j = 0; j < 32; j++) {
        int id2[] = {id, j};
        int trg2 = 0;
        if (!ht_get(vert_tri_lookup, id2, &trg2)) {
          ht_set(vert_tri_lookup, id2, &trg);
          break;
        } else if (trg2 == (int)trg) {
          // already added.
        }
      }
      ids[i] = id;
    }
    face_edge e1[] = {{.v1 = ids[0], .v2 = ids[1]},
                      {.v1 = ids[1], .v2 = ids[2]},
                      {.v1 = ids[2], .v2 = ids[0]}};
    int edge_ids[3];
    for (int i = 0; i < 3; i++) {
      if (e1[i].v1 > e1[i].v2)
        SWAP(e1[i].v1, e1[i].v2);
      int eid;
      if (!ht_get(edge_lookup, &e1[i], &eid)) {
        eid = edge_count;
        ht_set(edge_lookup, &e1[i], &eid);
        edge_count += 1;
        edges = realloc(edges, edge_count * sizeof(edges[0]));
        edges[edge_count - 1] = e1[i];

        edge_triangles =
            realloc(edge_triangles, edge_count * sizeof(edge_triangles[0]));
        edge_triangles[edge_count - 1] = (face_trg){0};
        edge_triangles[edge_count - 1].t1 = trg;
      } else {
        // printf("Reuse edge %i %i %i %i\n", eid, trg, edge_triangles[eid].t1,
        // edge_triangles[eid].t2);
        edge_triangles[eid].t2 = trg;
      }
      edge_ids[i] = eid;
    }
    if (edge_ids[0] == edge_ids[1] || edge_ids[1] == edge_ids[2] ||
        edge_ids[0] == edge_ids[2]) {

      // this triangle has the same edge twice.
      continue;
    }
    triangle_count += 1;
    triangles = realloc(triangles, triangle_count * sizeof(triangles[0]));
    for (int i = 0; i < 3; i++) {
      triangles[triangle_count - 1].verts[i] = ids[i];
      triangles[triangle_count - 1].edges[i] = edge_ids[i];
    }
  }
  var sdf = bld->sdf->sdf;
  var sdf_userdata = bld->sdf->sdf_userdata;
  vec3 c;

  if (false) {
    for (int i = 0; i < vertex_count; i++) {
      vec3 v = optimize_point(bld->sdf, vertexes[i], 0.0001f);
      vertexes[i] = v;
    }
  }
  if (false) {
    // optimize vertex positions.
    for (int i = 0; i < vertex_count; i++) {
      vec3 v = vertexes[i];
      vec3 connected[32] = {0};
      int j = 0;
      for (; j < 32; j++) {
        int id2[] = {i, j};
        int trg;
        if (!ht_get(vert_tri_lookup, id2, &trg)) {
          break;
        }
        var tri = triangles[trg];
        int con = -1;
        for (int k = 0; k < 3; k++) {
          if (edges[tri.edges[k]].v1 == i) {
            con = edges[tri.edges[k]].v2;
          } else if (edges[tri.edges[k]].v2 == i) {
            con = edges[tri.edges[k]].v1;
          }
        }
        if (con != -1)
          connected[j] = vertexes[con];
      }

      int cnt = j;
      vec3 u[] = {vec3_new(0, 0, 0), vec3_new(1, 0, 0),  vec3_new(-1, 0, 0),
                  vec3_new(0, 1, 0), vec3_new(0, -1, 0), vec3_new(0, 0, 1),
                  vec3_new(0, 0, -1)};
      f32 ers[7];
      f32 di = 0.5;
      for (int k = 0; k < 400; k++) {
        // printf("K: %i\n", k);
        // vec3_print(v), vec3_print(connected[0]);
        for (int j = 0; j < 7; j++) {
          vec3 u2 = vec3_scale(u[j], di);
          f32 e = 0;

          for (int i = 0; i < cnt; i++) {
            vec3 mid =
                vec3_scale(vec3_add(connected[i], vec3_add(v, u2)), 0.5f);
            var d2 = sdf(sdf_userdata, mid, &c);
            e += d2 * d2;
          }
          e = sqrtf(e);
          ers[j] = e;
        }
        // for(int i = 0; i < 7; i++)
        //   printf(" %f ", ers[i]);
        int min_i = 0;
        for (int j = 0; j < 7; j++) {
          if (ers[j] < ers[min_i])
            min_i = j;
        }
        // printf("E: %f\n", ers[min_i]);
        if (min_i == 0)
          di *= 0.5f;
        else
          v = vec3_add(v, vec3_scale(u[min_i], di));
        if (di < 0.0001f)
          break;
        // vec3_print(vd);
        // v = vec3_sub(v, vec3_scale(vd, 1.0));
      }
      vertexes[i] = v;

      // printf("Count :%i \n", cnt);
    }
  }

  skip_triangles = alloc0(sizeof(skip_triangles[0]) * triangle_count);
  int new_vertexes = 0;
  int edge_count2 = edge_count;
  printf("OPTIMIZE!\n");

  if (false) {
    for (int i = 0; i < edge_count2; i++) {
      var e = edges[i];
      var et = edge_triangles[i];
      var f1 = triangles[et.t1];
      var f2 = triangles[et.t2];
      printf("OPTIMIZE?\n");
      if (skip_triangles[et.t1])
        continue;
      if (skip_triangles[et.t2])
        continue;
      // only edges with two triangles connected can be split
      if (et.t1 == 0 || et.t2 == 0)
        continue;

      vec3 v1 = vertexes[e.v1];
      vec3 v2 = vertexes[e.v2];
      vec3 mid = vec3_scale(vec3_add(v1, v2), 0.5);
      vec3 c;
      var d = sdf(sdf_userdata, mid, &c);

      // vec3 asd = vec3_new(0.5432, -0.3234, 0.7783);
      // vec3 cross = vec3_normalize(vec3_mul_cross(vec3_sub(v2, v1), asd));
      if (fabsf(d) > 0.01f) {
        var m2 = mid;
        m2 = optimize_point(bld->sdf, mid, 0.01);
        vec3_print(v1);
        vec3_print(v2);
        vec3_print(mid);
        printf("\n");
        // for(int i = 0; i < 0; i++)
        //   m2 = trace_point(bld->sdf, m2, 0.01);
        new_vertexes++;
        int id;
        if (!ht_get(vertex_lookup, &m2.x, &id)) {
          id = vertex_count;
          ht_set(vertex_lookup, &m2.x, &id);
          vertex_count += 1;
          vertexes = realloc(vertexes, vertex_count * sizeof(vertexes[0]));
          vertexes[vertex_count - 1] = m2;
        }
        skip_triangles[et.t1] = true;
        skip_triangles[et.t2] = true;
        // one edge gets removed.
        //  all other edges still exist, but are connected with the new vertex.
        if (true) {
          int e1 = -1, e2 = -1, e3 = -1, e4 = -1;
          for (int j = 0; j < 3; j++) {
            if (f1.edges[j] != i) {
              if (e1 == -1)
                e1 = f1.edges[j];
              else
                e2 = f1.edges[j];
            }
            if (f2.edges[j] != i) {
              if (e3 == -1)
                e3 = f2.edges[j];
              else
                e4 = f2.edges[j];
            }
          }

          // these 4 edges, along with the center vertex form 4 new triangles.
          int new_edges[] = {e1, e2, e3, e4};
          for (int j = 0; j < 4; j++) {
            if (new_edges[j] == -1) {
              printf("%i % i %i %i\n", e1, e2, e3, e4);
              printf("%i % i %i\n", f1.edges[0], f1.edges[1], f1.edges[2]);
              ASSERT(false);
            }
            // printf("new triangle\n");
            face_edge e0 = edges[new_edges[j]];
            int ids[] = {e0.v1, e0.v2, id};
            int edge_ids[3];
            face_edge e1[] = {{.v1 = ids[0], .v2 = ids[1]},
                              {.v1 = ids[1], .v2 = ids[2]},
                              {.v1 = ids[2], .v2 = ids[0]}};
            for (int i = 0; i < 3; i++) {
              if (e1[i].v1 > e1[i].v2)
                SWAP(e1[i].v1, e1[i].v2);
              int eid;
              if (!ht_get(edge_lookup, &e1[i], &eid)) {
                eid = edge_count;
                ht_set(edge_lookup, &e1[i], &eid);
                edge_count += 1;
                edges = realloc(edges, edge_count * sizeof(edges[0]));
                edges[edge_count - 1] = e1[i];

                edge_triangles = realloc(
                    edge_triangles, edge_count * sizeof(edge_triangles[0]));
                edge_triangles[edge_count - 1] = (face_trg){0};
                edge_triangles[edge_count - 1].t1 = triangle_count;
                printf("new_edge\n");
              } else {
                printf(" edge %i %i %i %i\n", eid, triangle_count,
                       edge_triangles[eid].t1, edge_triangles[eid].t2);
                edge_triangles[eid].t2 = triangle_count;
              }
              edge_ids[i] = eid;
            }
            triangle_count += 1;
            triangles =
                realloc(triangles, triangle_count * sizeof(triangles[0]));
            skip_triangles = realloc(
                skip_triangles, triangle_count * sizeof(skip_triangles[0]));
            skip_triangles[triangle_count - 1] = false;
            // printf("new triangle: ");
            for (int i = 0; i < 3; i++) {
              triangles[triangle_count - 1].verts[i] = ids[i];
              triangles[triangle_count - 1].edges[i] = edge_ids[i];
              // vec3_print(vertexes[ids[i]]);
            }
            // printf("\n");
          }
          // printf("EDGE: %i %i %i %i\n", e1, e2, e3, e4);
        }
      }
    }
  }

  u8 *vertex_hits = alloc0(vertex_count);
  ;
  int skip = 0;
  for (int i = 0; i < triangle_count; i++) {
    var trig = triangles[i];
    if (skip_triangles[i])
      skip += 1;
    for (int j = 0; j < 3; j++) {
      vertex_hits[trig.verts[j]]++;
    }
  }

  {
    bld->out_verts = alloc0((triangle_count - skip) * 9 * sizeof(float));
    bld->out_colors = alloc0((triangle_count - skip) * 9 * sizeof(float));
    // each triangle has 3 vertes, each verts has 3 dimensions.
    f32 *vp = bld->out_verts;
    f32 *cp = bld->out_colors;
    bld->out_count = triangle_count * 3;
    for (int i = 0; i < triangle_count; i++) {
      if (skip_triangles[i]) {
        // printf("SKIP\n");
        continue;
      }
      var trig = triangles[i];
      vec3 _c2;
      for (int j = 0; j < 3; j++) {
        var vx = vertexes[trig.verts[j]];
        vec3 vx_c;
        sdf(sdf_userdata, vx, &vx_c);
        var vxup = vec3_add(vx, vec3_new(0, 0.3f, 0));
        sdf(sdf_userdata, vx, &vx_c);
        for (int i = 0; i < 20; i++) {
          float dup = sdf(sdf_userdata, vxup, &_c2);
          vxup = vec3_add(vxup, vec3_new(0, dup, 0));
        }
        if (vec3_len(vec3_sub(vxup, vx)) < 50.0f) {
          vx_c = vec3_scale(vx_c, 0.9f);
        }
        // vec3_print(vx); printf("\n");
        for (int k = 0; k < 3; k++) {
          vp[0] = vx.data[k];
          cp[0] = vx_c.data[k];
          vp++;
          cp++;
        }
      }
    }
  }

  printf("vert count: %i, edge count: %i, new_vertexes: %i\n", vertex_count,
         edge_count, new_vertexes);
}

hash_table *vertex_lookup2 = NULL;
int reuse1 = 0;
int reuse2 = 0;

f32 generic_sdf2(void *ud, vec3 p, vec3 *c) {
  // return generic_sdf(ud, p, c);
  if (false && vertex_lookup2 != NULL) {
    f32 r[4];
    if (!ht_get(vertex_lookup2, &p.x, r)) {
      vec3 c2;
      r[0] = generic_sdf(ud, p, &c2);
      r[1] = c2.x;
      r[2] = c2.z;
      r[3] = c2.y;
      ht_set(vertex_lookup2, &p.x, r);
      reuse1 += 1;
    } else {
      reuse2 += 1;
      // printf("reuse! %i %i\n", reuse1, reuse2);
    }
    if (c != NULL) {
      c->x = r[1];
      c->y = r[2];
      c->z = r[3];
    }

    return r[0];
  }
  // vec3_print(p);printf("\n");
  return generic_sdf(ud, p, c);
}
bool improve_meshes = true;
void ht_empty(hash_table *ht);
lisp_value sdf_marching_cubes(lisp_value scale_v, lisp_value _res,
                              lisp_value model0, lisp_value offset) {
  if (!is_nil(model0)) {
    void *model = get_physics_model_cached2(model0);
    if (model == NULL)
      return nil;
    // var timestamp0 = timestampf();
    // describe_sdf(model);

    var center = is_nil(offset) ? vec3_zero : lv_vec3(offset);

    var scale = lisp_value_as_rational(scale_v);
    f64 res = lisp_value_as_rational(_res);
    df_ctx ctx2 = {
        .sdf = generic_sdf2, .sdf_userdata = model, .threshold = res};

    mc_vertex_builder builder = {
        .verts = NULL, .colors = NULL, .count = 0, .offset = 0, .sdf = &ctx2};
    ctx2.userdata = &builder;
    ctx2.emit_point = mc_take_vertex;

    if (vertex_lookup2 != NULL) {
      ht_empty(vertex_lookup2);
      dealloc(vertex_lookup2);
      vertex_lookup2 = NULL;
    }

    vertex_lookup2 = alloc(sizeof(*vertex_lookup2));
    ht_create3(vertex_lookup2, 1, sizeof(vec3), sizeof(f32) * 4);
    vertex_lookup2->hash = (void *)vec3_hash;

    // printf("Marching cubes...\n");
    // var timestamp = timestampf();
    // for(int i = 0; i < 1000; i++){
    marching_cubes_sdf(&ctx2, center, scale);
    if (!improve_meshes) {
      var vec_v = make_vector(integer(builder.count * 3), float32(0.0));
      f32 *verts2 = vec_v.vector->data;
      var vec_c = make_vector(integer(builder.count * 3), float32(0.0));
      f32 *colors2 = vec_c.vector->data;
      memcpy(verts2, builder.verts, sizeof(float) * 3 * builder.count);
      var sdf = ctx2.sdf;
      var sdf_userdata = ctx2.sdf_userdata;

      for (size_t i = 0; i < builder.count; i++) {
        vec3 *v2 = (vec3 *)(verts2 + i * 3);
        vec3 vx_c;
        sdf(sdf_userdata, *v2, &vx_c);
        f32 *cp = colors2 + i * 3;
        cp[0] = vx_c.x;
        cp[1] = vx_c.y;
        cp[2] = vx_c.z;
      }
      dealloc(builder.verts);
      builder.verts = NULL;
      return new_cons(vec_v, vec_c);
    }
    // builder.offset = 0;
    // }

    // printf("Done! %f\n", (f32)(timestampf() - timestamp));
    builder.count = builder.offset;
    // timestamp = timestampf();
    improve_mesh(&builder);
    dealloc(builder.verts);
    // printf("improve mesh! %f\n", (f32)(timestampf() - timestamp));

    var vec3 = make_vector(integer(builder.out_count * 3), float32(0.0));
    f32 *verts2 = vec3.vector->data;
    var vec4 = make_vector(integer(builder.out_count * 3), float32(0.0));
    f32 *colors2 = vec4.vector->data;
    memcpy(verts2, builder.out_verts, sizeof(float) * 3 * builder.out_count);
    memcpy(colors2, builder.out_colors, sizeof(float) * 3 * builder.out_count);
    // println(vec3);
    // printf("POINTS: %i: %f %i\n", (int)builder.out_count,
    // (float)(timestampf() - timestamp), (int)vertex_lookup2->count);
    // printf("finished: %f\n",  (float)(timestampf() - timestamp0));

    return new_cons(vec3, vec4);
  }
  vec3 center = vec3_new(0, 0.5, 0);
  f32 scale = 4.0;
  sdf_color c1 = {.type = SDF_TYPE_COLOR,
                  .color = vec3_new(0.5, 0.9, 0.4),
                  .model_count = 2};
  sdf_color c2 = {.type = SDF_TYPE_COLOR,
                  .color = vec3_new(0.5, 0.4, 0.2),
                  .model_count = 1};
  sdf_aabb a = {.type = SDF_TYPE_AABB,
                .pos = vec3_new(0, -10.5, 0),
                .size = vec3_new(100, 10, 100)};
  sdf_vert_capsule cap1 = {.type = SDF_TYPE_VERT_CAPSULE,
                           .height = 3.0,
                           .radius = 0.5,
                           .pos = vec3_new(0, -1, 0)};

  sdf_sphere s1 = {
      .type = SDF_TYPE_SPHERE, .pos = vec3_new(0.0, 3, 0), .radius = 1.5};
  sdf_sphere s2 = {
      .type = SDF_TYPE_SPHERE, .pos = vec3_new(1.0, 2.0, 0), .radius = 1.0};
  sdf_sphere s3 = {
      .type = SDF_TYPE_SPHERE, .pos = vec3_new(-1.0, 2.0, 0), .radius = 1.0};
  sdf_sphere s4 = {
      .type = SDF_TYPE_SPHERE, .pos = vec3_new(0, -50, 0), .radius = 50};

  void *models3[] = {&s1, &s2, &s3};
  c1.models = models3;
  void *models2[] = {&cap1};
  c2.models = models2;
  void *models[] = {&c1, &c2, &s4};
  sdf_models ms = {.type = SDF_TYPE_MODELS, .model_count = 3, .models = models};
  sdf_model model = {0};
  model.sdf = generic_sdf;
  model.userdata = &a;
  model.sdf = generic_sdf;
  model.userdata = &ms;

  size_t count = 0;
  df_ctx ctx2 = {.sdf = sdf_model_sdf,
                 .sdf_userdata = &model,
                 .threshold = 0.1,
                 .emit_point = mc_count_vertexes,
                 .userdata = &count};

  marching_cubes_sdf(&ctx2, center, scale);

  var vec = make_vector(integer(count * 3 * 3), float32(0.0));
  f32 *verts = vec.vector->data;
  var vec2 = make_vector(integer(count * 3 * 3), float32(0.0));
  f32 *colors = vec2.vector->data;
  mc_vertex_builder builder = {
      .verts = verts, .colors = colors, .count = count, .offset = 0};
  ctx2.userdata = &builder;
  ctx2.emit_point = mc_take_vertex;

  marching_cubes_sdf(&ctx2, center, scale);

  printf("POINTS: %i\n", (int)count);

  return new_cons(vec, vec2);
}

typedef struct {
  blit3d_polygon *vert;
  blit3d_polygon *color;
  int count;
} sdf_poly_t;

extern blit3d_context *blit3d_current_context;
int render_sdf_lods2(df_ctx *model, mat4 tform, vec3 center, vec3 offset,
                     f32 scale, int level) {
  static hash_table *poly_lookup;
  if (poly_lookup == NULL) {
    poly_lookup = alloc(sizeof(*poly_lookup));
    ht_create3(poly_lookup, 1, sizeof(vec4), sizeof(sdf_poly_t));
  }
  var offset2 = offset;
  var d2 = vec3_len(vec3_sub(offset2, center));
  int lod = floorf(powf(d2 / 20, 0.5f));
  // vec3_print(offset2); printf("scale: %f %f %i (%f)\n", scale, d2, lod, d2 /
  // 5); vec3 c; f32 d = model->sdf(model->sdf_userdata, offset2, &c); if(d >
  // scale * sqrt_3 * 1.5)
  //   return 0;
  if (level <= lod) { // || vec3_len(vec3_sub(offset2, center)) > scale * 10){
    // printf("Render chunk: %i %f\n", level, vec3_len(vec3_sub(offset2,
    // center)));
    //  render this LOD
    vec4 key = vec4_new(offset2.x, offset2.y, offset2.z, (f32)lod);
    sdf_poly_t poly = {0};
    if (ht_get(poly_lookup, &key, &poly) == false) {
      // vec3_print(p); printf("%f   <--- \n", scale);

      mc_vertex_builder builder = {
          .verts = NULL, .colors = NULL, .count = 0, .offset = 0, .sdf = model};
      model->userdata = &builder;
      model->emit_point = mc_take_vertex;
      model->threshold = scale / 4.0f;

      marching_cubes_sdf(model, offset2, scale * 0.55f);
      builder.count = builder.offset;
      if (builder.count > 0) {

        // timestamp = timestampf();
        improve_mesh(&builder);
        dealloc(builder.verts);

        var poly_v = blit3d_polygon_new();
        blit3d_polygon_load_data(poly_v, builder.out_verts,
                                 sizeof(float) * 3 * builder.out_count);
        blit3d_polygon_configure(poly_v, 3);

        var poly_c = blit3d_polygon_new();
        blit3d_polygon_load_data(poly_c, builder.out_colors,
                                 sizeof(float) * 3 * builder.out_count);
        blit3d_polygon_configure(poly_c, 3);
        poly.vert = poly_v;
        poly.color = poly_c;
      }
      poly.count = builder.count;
      ht_set(poly_lookup, &key, &poly);
    }
    if (poly.vert != NULL) {
      blit3d_view(blit3d_current_context, tform);
      blit3d_set_mode(blit3d_current_context, BLIT3D_TRIANGLES_COLOR);

      blit3d_polygon_blit2(blit3d_current_context, &poly.vert, 2);
      return poly.count;
    }
    return 0;
  }
  if (level == 0)
    return 0;
  int verts = 0;
  // vec3_print(center2);printf("%f \n", scale);
  for (int i = -1; i < 2; i++) {
    for (int j = -1; j < 2; j++) {
      for (int k = -1; k < 2; k++) {
        vec3 p = vec3_add(offset2, vec3_scale(vec3_new(i, j, k), scale / 3.0f));
        // vec3_print(p);printf("\n");
        verts +=
            render_sdf_lods2(model, tform, center, p, scale / 3.0f, level - 1);
      }
    }
  }
  return verts;
}

lisp_value lisp_render_sdf_lods(lisp_value model, lisp_value transform,
                                lisp_value center) {
  // lets say we render 4 levels of detail, with a 10x10x10 chunk being the
  // lowest level of detail and 2^4 = 16 * 10 160x160x160 chunk being the
  // biggest
  printf("Render SDF\n");
  int maxLevel = 3;

  f32 max_scale = powf(2.0, maxLevel);
  vec3 center_pos = lv_vec3(center);
  void *modelp = get_physics_model_cached2(model);
  df_ctx ctx2 = {.sdf = generic_sdf2, .sdf_userdata = modelp};

  var init_scale = max_scale * 3 * 3 * 2;
  int count = 0;
  var offset0 = vec3_scale(
      vec3_round(vec3_scale(center_pos, 1.0f / init_scale)), init_scale);
  vec3_print(offset0);
  printf("init scale: %lf\n", (f64)init_scale);
  var tform = lisp_value_mat4(transform);
  for (int i = -2; i < 3; i++) {
    for (int j = -2; j < 3; j++) {
      for (int k = -2; k < 3; k++) {
        vec3 p = vec3_add(offset0, vec3_scale(vec3_new(i, j, k), init_scale));
        count +=
            render_sdf_lods2(&ctx2, tform, center_pos, p, init_scale, maxLevel);
      }
    }
  }
  // then subdivide the space into 27 chunks, whichever of those chunks includes
  // the center or is right next to it, gets subdivided recursively
  // int count = render_sdf_lods2(&ctx2, , center_pos, offset0,init_scale ,
  // maxLevel);
  printf("Render SDF Vertexes: %i\n", count);

  return nil;
}

lisp_value lisp_sdf_distance(lisp_value model, lisp_value p) {
  if (is_nil(model))
    return nil;
  void *modelp = get_physics_model_cached2(model);
  if (modelp == NULL) {
    return nil;
  }
  vec3 c;
  vec3 p2 = is_list(p) ? lv_vec3(p) : lisp_value_vec3(p);
  return rational_lisp_value((double)generic_sdf(modelp, p2, &c));
}

lisp_value lisp_sdf_distance_gradient(lisp_value model, lisp_value p,
                                      lisp_value size) {
  if (is_nil(model))
    return nil;
  void *modelp = get_physics_model_cached2(model);
  if (modelp == NULL) {
    return nil;
  }
  f32 s = is_nil(size) ? 0.01f : (f32)lisp_value_as_rational(size);
  vec3 c;

  vec3 p2 = is_list(p) ? lv_vec3(p) : lisp_value_vec3(p);

  var d0 = generic_sdf(modelp, p2, &c);

  var dx = generic_sdf(modelp, vec3_add(vec3_new(s, 0.0f, 0.0f), p2), &c);
  var dy = generic_sdf(modelp, vec3_add(vec3_new(0.0f, s, 0.0f), p2), &c);
  var dz = generic_sdf(modelp, vec3_add(vec3_new(0.0f, 0.0f, s), p2), &c);

  return vec3_lisp_value(
      vec3_normalize(vec3_sub(vec3_new(dx, dy, dz), vec3_new1(d0))));
}

void lrn(const char *l, int args, void *f);

typedef struct {
  int count;
} triangles_builder;

void test_marching_cubes_f(void *userdata, vec3 pt, vec3 color) {
  UNUSED(color);
  triangles_builder *b = userdata;
  b->count += 1;
  printf("Vertex: ");
  vec3_print(pt);
  printf("\n");
}
void test_marching_cubes() {
  sdf_aabb a = {.pos = vec3_new(0, 0, 0), .size = vec3_new(0.5, 0.5, 0.5)};
  sdf_model model = {0};
  model.sdf = aabb_sdf;
  model.userdata = &a;
  process_cube(&model, vec3_new(-0.5, -0.0, -0.0), 0.2, print_triangle, NULL);

  triangles_builder b = {0};
  df_ctx ctx2 = {.sdf = sdf_model_sdf,
                 .sdf_userdata = &model,
                 .threshold = 0.001,
                 .emit_point = test_marching_cubes_f,
                 .userdata = &b};
  marching_cubes_sdf(&ctx2, vec3_new(0.1, 0.1, 0.1), 2.0);
  printf("Vertex count: %i\n", b.count);
}

void test_sdf() {
  test_sdf_col();
  printf("test layered sdf\n");
  test_layered_sdf();
  printf("Marching Cubes\n");
  // sdf_poly(nil);
  test_marching_cubes();
}

lisp_value lisp_render_sdf_to_image(lisp_value model,
                                    lisp_value model_transform,
                                    lisp_value camera_transform,
                                    lisp_value width, lisp_value height,
                                    lisp_value depth) {
  // lets say we render 4 levels of detail, with a 10x10x10 chunk being the
  // lowest level of detail and 2^4 = 16 * 10 160x160x160 chunk being the
  // biggest
  void *modelp = get_physics_model_cached2(model);
  if (modelp == 0) {
    raise_string("Unable to load physics model");
    return nil;
  }
  df_ctx ctx = {.sdf = generic_sdf2, .sdf_userdata = modelp};

  int w = lisp_value_integer_checked(width);
  int h = lisp_value_integer_checked(height);
  var d = lisp_value_as_rational(depth);
  var model_tform = lisp_value_mat4(model_transform);
  var model_tform_i = mat4_invert(model_tform);
  var cam_tform = lisp_value_mat4(camera_transform);
  var cam_tform_i = mat4_invert(cam_tform);
  u8 *image = lisp_malloc(w * h * 4);
  f32 *dimage = lisp_malloc(w * h * 4);

  // var campos = mat4_mul_vec3(cam_tform, vec3_new(0,0,0));
  mat4_print(cam_tform);

  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
      var vpx = vec3_new(i, j, 1);
      vpx.x = (2.0f * vpx.x / w) - 1.0f;
      vpx.y = (2.0f * vpx.y / h) - 1.0f;
      var cpx = mat4_mul_vec3(cam_tform_i, vpx);
      var dvec = vec3_new(0, 0, -1);
      cpx = vec3_sub(cpx, vec3_scale(dvec, d));
      var cpx0 = cpx;
      // vec3_print(cpx);
      vec3 color = {0};
      float d0 = 10000;
      cpx = mat4_mul_vec3(model_tform_i, cpx);
      while (d0 > 0.00001f && d0 < 20000.0f) {
        d0 = ctx.sdf(ctx.sdf_userdata, cpx, &color);
        cpx = vec3_add(cpx, vec3_scale(dvec, d0));
      }
      int idx = i + j * w;
      dimage[idx] = vec3_len(vec3_sub(cpx, cpx0));
      if (d0 < 0.0001f) {
        printf("#");
        for (int k = 0; k < 3; k++)
          image[idx + k] = (u8)(color.data[k] * 255);
        image[idx + 3] = 255;

      } else {
        printf(" ");
        for (int k = 0; k < 4; k++)
          image[idx + k] = 0;
      }
    }
    printf("\n");
  }

  lisp_vector *image_vec = lisp_malloc(sizeof(lisp_vector));
  image_vec->data = image;
  image_vec->count = w * h * 4;
  image_vec->elem_size = 1;
  image_vec->default_value = byte_lisp_value(0);

  lisp_vector *depth_vec = lisp_malloc(sizeof(lisp_vector));
  depth_vec->data = dimage;
  depth_vec->count = w * h;
  depth_vec->elem_size = 4;
  depth_vec->default_value = float32_lisp_value(0.0f);

  return new_cons(vector_lisp_value(image_vec), vector_lisp_value(depth_vec));
}

void sdf_register() {
  lrn("sdf:dist", 2, lisp_sdf_distance);
  lrn("sdf:dist-gradient", 3, lisp_sdf_distance_gradient);
  lrn("sdf:render", 3, lisp_render_sdf_lods);
  lrn("sdf:to-image", 6, lisp_render_sdf_to_image);
  lrn("sdf:detect-collision", 4, foxgl_detect_collision2);
  lrn("sdf:detect-collision2", 5, foxgl_detect_collision3);
  
}
