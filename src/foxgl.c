#include <iron/full.h>
#include <iron/gl.h>
#include "foxlisp.h"
#include <GL/gl.h>

lisp_value rect2(lisp_value r, lisp_value g,lisp_value b,lisp_value a){
  blit_rectangle2(r.rational,g.rational,b.rational,a.rational);
  return nil;
}

lisp_value scale(lisp_value x, lisp_value y){
  blit_scale(x.rational, y.rational);
  return nil;
}

lisp_value translate(lisp_value x, lisp_value y){
  blit_translate(x.rational, y.rational);
  return nil;
}

lisp_value color(lisp_value r, lisp_value g, lisp_value b, lisp_value a){
  blit_color(r.rational, g.rational, b.rational, a.rational);
  return nil;
}

lisp_value set_texture(lisp_value r){
  blit_bind_texture((texture *)r.native_pointer);
  return nil;
}

lisp_value quad(){
  blit_quad();
  return nil;
}

lisp_value text(lisp_value str){
  blit_text(str.string);
  return nil;
}

lisp_value load_font(lisp_value str, lisp_value size){
  const char * fontfile_init = str.string;
  //printf("Load font: %s %i\n", str.string, size.integer);
  var fnt0 =  blit_load_font_file(fontfile_init,  size.integer);
  blit_set_current_font(fnt0);
  return nil;
}

lisp_value load_texture_from_path(lisp_value str){
  image img2 = image_from_file(str.string);
  if(img2.source == NULL) return nil;

  texture duck_tex = texture_from_image(&img2);
  image_delete(&img2);
  texture * t = lisp_malloc(sizeof(duck_tex));
  *t = duck_tex;
  return (lisp_value){.type = LISP_NATIVE_POINTER, .native_pointer = t};
}

typedef enum{
				 DF_NULL,
				 DF_COLOR,
				 DF_SPHERE,
				 DF_TRIANGLE,
				 DF_TRANSFORM
}df_type;

typedef struct{
  int color_rgba;
}df_color;

typedef struct{
  vec3 center;
  float radius;
}df_sphere;

typedef struct{
  vec3 a, b, c;
}df_triangle;

typedef struct{
  mat4 transform;
}df_transform;

typedef struct _df_value df_value;
struct _df_value{
  df_type type;
  union{
	 df_color color;
	 df_sphere sphere;
	 df_triangle triangle;
	 df_transform transform;
  };
  df_value * sub_items;
  size_t count;
};

df_value df_null = {.type = DF_NULL};

lisp_value plist_find(lisp_value cons, lisp_value sym){
  while(cons.type == LISP_CONS){
	 if(eq(car(cons), sym))
		return cadr(cons);
	 if(car(cons).type == LISP_SYMBOL){
		cons = cddr(cons);
	 }else{
		cons = cdr(cons);
	 }
  }
  return nil;
}

bool try_rational(float * out, lisp_value v){
  if(v.type == LISP_RATIONAL){
	 *out = v.rational;
  }else if(v.type == LISP_INTEGER){
	 *out = v.integer;
  }else return false;
  printf("YES: %f\n", *out);
  return true;
}

bool list_vec3(lisp_value c, vec3 * out){
  
  vec3 v;
  if(try_rational(&v.x, POP(c)) && try_rational(&v.y, POP(c)) && try_rational(&v.z, POP(c))){
	 printf("%f %f %f\n",v.x,v.y,v.z);printf("\n");
	 *out = v;
	 return true;
  }
  return false;  
}


bool list_vec4(lisp_value c, vec4 * out){
  
  vec4 v;
  if(try_rational(&v.x, POP(c)) && try_rational(&v.y, POP(c)) && try_rational(&v.z, POP(c)) && try_rational(&v.w, POP(c))){
	 *out = v;
	 return true;
  }
  return false;  
}
bool plist_vec3(lisp_value c, lisp_value sym, vec3 * out){
  c = plist_find(c, sym);
  if(is_nil(c)) return false;
  return list_vec3(c, out);	
}


bool plist_rgb(lisp_value c, lisp_value sym, int * out){
  c = plist_find(c, sym);
  if(is_nil(c)) return false;
  vec3 p = {0};
  if(!list_vec3(c, &p)) return false;
  union{
	 unsigned char bytes[4];
	 int integer;
  }color;
  color.bytes[0] = (unsigned char) (p.x * 255);
  color.bytes[1] = (unsigned char) (p.y * 255);
  color.bytes[2] = (unsigned char) (p.z * 255);
  color.bytes[3] = 0xFF;
  *out = color.integer;
  return true;
}

bool plist_rgba(lisp_value c, lisp_value sym, int * out){
  c = plist_find(c, sym);
  if(is_nil(c)) return false;
  vec4 p = {0};
  if(!list_vec4(c, &p)) return false;
  union{
	 unsigned char bytes[4];
	 int integer;
  }color;
  color.bytes[0] = (unsigned char) (p.x * 255);
  color.bytes[1] = (unsigned char) (p.y * 255);
  color.bytes[2] = (unsigned char) (p.z * 255);
  color.bytes[3] = (unsigned char) (p.w * 255);
  *out = color.integer;
  return true;
}
bool plist_f32(lisp_value c, lisp_value sym, float * out){
  c = plist_find(c, sym);
  if(is_nil(c)) return false;
  return try_rational(out, c);
}

lisp_value sphere_sym;
lisp_value color_sym;
lisp_value center_sym, radius_sym, rgb_sym, rgba_sym;
lisp_value triangle_sym, transform_sym;
lisp_value a_sym, b_sym, c_sym;
lisp_value translate_sym;
df_value build_df(lisp_value v){
  printf("build df\n");
  var c = car(v);
  v = cdr(v);
  df_value value = {0};

  if(eq(c, sphere_sym)){
	 value.type = DF_SPHERE;
	 plist_vec3(v, center_sym, &value.sphere.center);
	 plist_f32(v, radius_sym, &value.sphere.radius);
	 printf("--- %f \n", value.sphere.radius);
  }else if(eq(c, color_sym)){
	 value.type = DF_COLOR;
	 plist_rgb(v, rgb_sym, &value.color.color_rgba);
	 plist_rgba(v, rgba_sym, &value.color.color_rgba);
  }else if(eq(c, triangle_sym)){
	 value.type = DF_TRIANGLE;
	 plist_vec3(v, a_sym, &value.triangle.a);
	 plist_vec3(v, b_sym, &value.triangle.b);
	 plist_vec3(v, c_sym, &value.triangle.c);
	 
  }else if(eq(c, transform_sym)) {
	 vec3 translate;
	 if(plist_vec3(v, translate_sym, &translate)){
		value.transform.transform = mat4_translate(translate.x ,translate.y, translate.y);
	 }
  }
  df_value * subs = NULL;
  size_t subcnt = 0;
  while(!is_nil(v)){

	 if(car(v).type == LISP_CONS){
		df_value sub = build_df(car(v));
		v = cdr(v);
		subcnt += 1;
		subs = lisp_realloc(subs, subcnt * sizeof(subs[0]));
		subs[subcnt - 1] = sub;
		continue;
	 }
	 
	 if(car(v).type == LISP_SYMBOL){
		v = cddr(v);
	 }else{
		v = cdr(v);
	 }
  }
  value.sub_items = subs;
  value.count = subcnt;
  return value;
}

lisp_value lisp_build_df(lisp_value v){
  sphere_sym = get_symbol("sphere");
  color_sym = get_symbol("color");
  center_sym = get_symbol(":center");
  radius_sym = get_symbol(":radius");
  rgb_sym = get_symbol(":rgb");
  rgba_sym = get_symbol(":rgba");
  center_sym = get_symbol(":center");
  radius_sym = get_symbol(":radius");
  triangle_sym = get_symbol("triangle");
  transform_sym = get_symbol("transform");
  a_sym = get_symbol(":a");
  b_sym = get_symbol(":b");
  c_sym = get_symbol(":c");
  var c = car(v);
  df_value d = build_df(v);
  df_value * p = lisp_malloc(sizeof(d));
  *p = d;
  return (lisp_value){.type = LISP_NATIVE_POINTER, .native_pointer = p};
}

void df_print(df_value * df){
  printf("(");
  switch(df->type){
  case DF_SPHERE:
	 {
		var center = df->sphere.center;
		var radius = df->sphere.radius;
		printf("sphere (%f %f %f) %f ", center.x , center.y, center.z, radius);
		break;
	 }
	 
  case DF_COLOR:
	 {
		var color = df->color.color_rgba;
		printf("color %p ", color);
	 }
	 break;
  case DF_TRIANGLE:{
	 printf("triangle ");
  }
	 break;
  }
  for(size_t i = 0; i < df->count;  i++)
	 df_print(df->sub_items + i);

  printf(")");
}

lisp_value lisp_print_df(lisp_value v){
  if(v.type != LISP_NATIVE_POINTER) return nil;
  var df = (df_value *) v.native_pointer;
  df_print(df);
  return nil;
}

float vec3_dot(vec3 a, vec3 b){
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

float vec3_dot2(vec3 a){
  return vec3_dot(a, a);
}


float sign(float x){
  if(x < 0) return -1;
  return 1;
}
float clampf(float a, float minv, float maxv){
  return MAX(MIN(a, maxv), minv);
}
float minf(float a, float b){
  return MIN(a,b);
}
float dist_triangle(df_triangle * t, float x, float y, float z){
  vec3 a = t->a, b = t->b, c = t->c;
  vec3 p = vec3_new(x, y, z);
  vec3 ba = vec3_sub(b, a); vec3 pa = vec3_sub(p, a);
  vec3 cb = vec3_sub(c, b); vec3 pb = vec3_sub(p, b);
  vec3 ac = vec3_sub(a, c); vec3 pc = vec3_sub(p, c);
  vec3 nor = vec3_mul_cross( ba, ac );
  
  return sqrtf(
					(sign(vec3_dot(vec3_mul_cross(ba,nor),pa)) +
					 sign(vec3_dot(vec3_mul_cross(cb,nor),pb)) +
					 sign(vec3_dot(vec3_mul_cross(ac,nor),pc))<2.0)
	 ?
	 minf( minf(
				 vec3_dot2(vec3_sub(vec3_scale(ba,clampf(vec3_dot(ba,pa)/vec3_dot2(ba),0.0,1.0)),pa)),
				 vec3_dot2(vec3_sub(vec3_scale(cb, clampf(vec3_dot(cb,pb)/vec3_dot2(cb),0.0,1.0)),pb)) ),
			vec3_dot2(vec3_sub(vec3_scale(ac,clampf(vec3_dot(ac,pc)/vec3_dot2(ac),0.0,1.0)),pc)) )
     :
     vec3_dot(nor,pa)*vec3_dot(nor,pa)/vec3_dot2(nor) );
}

float df_dist(df_value * df, float x, float y, float z, int * color){
  float d = 100000;
  if(df->type == DF_SPHERE){
	 var c = df->sphere.center;
	 c.x = x - c.x;
	 c.y = y - c.y;
	 c.z = z - c.z;
	 c.x *= c.x;
	 c.y *= c.y;
	 c.z *= c.z;
	 float d2 = sqrtf(c.x + c.y + c.z) - df->sphere.radius;
	 d = MIN(d, d2);
  }
  if(df->type == DF_COLOR){
	 *color = df->color.color_rgba;
  }
  if(df->type == DF_TRIANGLE){
	 float d2 = dist_triangle(&df->triangle, x, y, z);
	 d = MIN(d, d2);
  }
  
  for(size_t i = 0; i < df->count; i++){
	 int c = *color;
	 float d2 = df_dist(df->sub_items + i, x, y, z, &c);
	 if(d2 < d){
		*color = c;
		d = d2;
	 }
  }
  return d;
}

float df_ray(df_value * df, float dx, float dy, float dz, int * color){
  float x = 0, y = 0, z = 0;
  float d = 0;
  for(int i = 0; i < 32; i++){
	 float d1 = df_dist(df, x, y, z, color);
	 if(d1 <= 0.1)
		return d1;
	 if(d1 > 1000)
		return d1;
	 d += d1;
	 x += d1 * dx;
	 y += d1 * dy;
	 z += d1 * dz;
  }
  return d;
}

lisp_value lisp_blit_distance_field(lisp_value tex, lisp_value _w, lisp_value _h, lisp_value df){
  //type_assert(tex, LISP_NATIVE_POINTER);
  type_assert(df, LISP_NATIVE_POINTER);
  type_assert(_w, LISP_INTEGER);
  type_assert(_h, LISP_INTEGER);
  int w = _w.integer;
  int h = _h.integer;
  df_value * d = df.native_pointer;
  image img = image_new(w, h, 4);
  u32 * data = image_data(&img);
  for(int i = 0; i < h; i++){
	 for(int j = 0; j < w; j++){
		int index = j + i * w;
		float dx = 2.0 * (float)j / w - 1.0;
		float dy = 2.0 * (float)i / h - 1.0;
		float dz = 0.5;
		float n = sqrtf(dx * dx + dy * dy + dz * dz);
		dx /= n;
		dy /= n;
		dz /= n;
		int color = 0;
		float dist = df_ray(d, dx, dy, dz, &color);
		if(dist <= 0.3)
		  data[index] = (u32)color;
		else
		  data[index] = 0x00000000;
	 }
  }
  image_save(&img, "test.img.png");
  image_delete(&img);
}

typedef enum {
				  MODEL_NONE = 0,
				  MODEL_TRANSFORM = 1,
				  MODEL_COLOR,
				  MODEL_TEXT,
				  MODEL_TRIANGLE,
				  MODEL_POLYGON,
				  MODEL_TEXTURE
}lisp_model_type;

const char * model_type_names[] =
  {"transform", "color", "text", "triangle", "polygon", "texture"};

lisp_model_type lisp_model_type_from_symbol(lisp_value c){
  static lisp_scope * model_type_lookup;
  ASSERT(c.type == LISP_SYMBOL);
  if(model_type_lookup == NULL){
	 model_type_lookup = lisp_scope_new(NULL);
	 for(int i = 0; i < array_count(model_type_names); i++){
       lisp_scope_set_value(model_type_lookup, get_symbol(model_type_names[i]), integer(i + 1));
    }
  }
  lisp_value r = {0};
  if(lisp_scope_try_get_value(model_type_lookup, c, &r))
	 return r.integer;
  return 0;
  
}

lisp_value lisp_blit_model(lisp_value model){
  var c = car(model);
  var v = cdr(model);
  lisp_model_type type = lisp_model_type_from_symbol(c);
  switch(type){
  case MODEL_COLOR:
	 
	 break;

  default:
	 break;
  }

  return nil;
}

lisp_value lmat4_mul(lisp_value a, lisp_value b){
  type_assert(a, LISP_VECTOR);
  type_assert(b, LISP_VECTOR);
  type_assert(a.vector->default_value, LISP_FLOAT32);
  type_assert(b.vector->default_value, LISP_FLOAT32);
  if(a.vector->count == 16){
	 mat4 * m1 = a.vector->data;
	 if(b.vector->count == 16){
		mat4 * m2 = b.vector->data;
		lisp_value r = vector_copy(a);
		mat4 * m3 = r.vector->data;
		*m3 = mat4_mul(*m1, *m2);
		return r;
	 }
	 error_print("Invalid number of rows and columns");	
  }else if(a.vector->count == 9){
	 mat3 * m1 = a.vector->data;
	 if(b.vector->count == 9){
		mat3 * m2 = b.vector->data;
		lisp_value r = vector_copy(a);
		mat3 * m3 = r.vector->data;
		*m3 = mat3_mul(*m1, *m2);
		return r;
	 }
  }

  error_print("Invalid number of rows and columns");
  return a;
}
lisp_value mat4_to_lisp (mat4 a);
lisp_value lmat4_rotate(lisp_value x, lisp_value y, lisp_value z){
  mat4 m1 = mat4_identity();
  if(is_nil(y)){
    m1 = mat4_rotate_Z(m1, z.rational);
  }else{
    m1 = mat4_rotate_X(m1, x.rational);
    m1 = mat4_rotate_Y(m1, y.rational);
    m1 = mat4_rotate_Z(m1, z.rational);
  }
  //mat4_print(m1);printf(" %f %f %f \n", x.rational, y.rational, z.rational);
  return mat4_to_lisp(m1);
}

#define TYPE_ASSERT(val, tp) type_assert(val, tp);

lisp_value lmat4_perspective(lisp_value fov, lisp_value aspect, lisp_value near, lisp_value far){
  TYPE_ASSERT(fov, LISP_RATIONAL);
  TYPE_ASSERT(aspect, LISP_RATIONAL);
  TYPE_ASSERT(near, LISP_RATIONAL);
  TYPE_ASSERT(far, LISP_RATIONAL);
  mat4 p = mat4_perspective(fov.rational, aspect.rational, near.rational, far.rational);
  return mat4_to_lisp(p);
}
           
lisp_value lmat4_orthographic(lisp_value w, lisp_value h, lisp_value z){
  TYPE_ASSERT(w, LISP_RATIONAL);
  TYPE_ASSERT(h, LISP_RATIONAL);
  TYPE_ASSERT(z, LISP_RATIONAL);
  mat4 p = mat4_ortho(-w.rational, w.rational, -h.rational, h.rational, -z.rational, z.rational);
  return mat4_to_lisp(p);
}

mat4 lisp_to_mat4(lisp_value a){
  type_assert(a, LISP_VECTOR);
  type_assert(a.vector->default_value, LISP_FLOAT32);
  if(a.vector->count == 16){
	 mat4 * m1 = a.vector->data;
	 return *m1;
  }else if(a.vector->count == 9){
	 //mat3 * m1 = a.vector->data;
	 
	 //return m0;
  }

  error_print("Invalid number of rows and columns");
  return mat4_identity();
}
lisp_value make_vector(lisp_value len, lisp_value _default);

lisp_value mat4_to_lisp (mat4 a){
  lisp_value vec = make_vector(integer(16), (lisp_value){.type = LISP_FLOAT32, .rational = 0.0f});
  float * data = &a;
  mat4 * v = vec.vector->data;
  *v = a;
  return vec;
}


lisp_value lmat4_print(lisp_value a){
  type_assert(a, LISP_VECTOR);
  type_assert(a.vector->default_value, LISP_FLOAT32);
  if(a.vector->count == 16){
	 mat4 * m1 = a.vector->data;
	 mat4_print(*m1);
	 return a;
  }else if(a.vector->count == 9){
	 mat3 * m1 = a.vector->data;
	 mat3_print(*m1);
	 return a;
  }

  error_print("Invalid number of rows and columns");
  return a;
}

float float32_2(lisp_value c, float def){
  if(c.type == LISP_RATIONAL || c.type == LISP_FLOAT32)
	 return c.rational;
  if(c.type == LISP_INTEGER || c.type == LISP_BYTE)
	 return (float)c.integer;
  return def;

}

vec4 color_from_u32(u32 integer){
  union {
	 u32 color1;
	 u8 colors[4];
  }x;
  x.color1 = integer;
  vec4 r;
  for(int i = 0; i < 4; i++)
	 r.data[i] = ((f32)x.colors[i])  / 256.0f;
  return r;
}

vec4 lisp_to_color(lisp_value color){
  vec4 r = {0};
  switch(color.type){
  case LISP_CONS:
	 r.x = float32_2(car(color), 1.0);
	 r.y = float32_2(cadr(color), 1.0);
	 r.z = float32_2(caddr(color), 1.0);
	 r.w = float32_2(cadddr(color), 1.0);
	 return r;
	 
  case LISP_INTEGER:
	 return color_from_u32(color.integer);
	 
  default:
	 break;
  }
  return r;
}
blit3d_context * blit3d_current_context = NULL;

lisp_value lblit_color(lisp_value color){
  vec4 col = lisp_to_color(color);
  if(blit3d_current_context != NULL)
	 blit3d_color(blit3d_current_context, col);
  return nil;
}

lisp_value lblit_transform(lisp_value tform1){
  mat4 tform = lisp_to_mat4(tform1);
  if(blit3d_current_context != NULL)
	 blit3d_view(blit3d_current_context, tform);
  return nil;
}

lisp_value lblit_init(){
  if(blit3d_current_context == NULL)
	 blit3d_current_context = blit3d_context_new();
  blit3d_context_load(blit3d_current_context);
  //glDisable(GL_BLEND);
  return nil;
}

lisp_value lblit_square2(){
  static blit3d_polygon * square;
  if(blit3d_current_context == NULL) return nil;
  if(square == NULL){
	 square = blit3d_polygon_new();
	 f32 pts[] = {0, 0, 1, 0, 0, 1, 1, 1};
	 blit3d_polygon_load_data(square, pts, sizeof(pts));
	 blit3d_polygon_configure(square, 2);
  }
  blit3d_polygon_blit2(blit3d_current_context, &square, 1);
  return nil;
}

lisp_value load_polygon (lisp_value val){
  if(blit3d_current_context == NULL) return nil;
  type_assert(val, LISP_VECTOR);
  type_assert(val.vector->default_value, LISP_FLOAT32);
  var pts = (float *) val.vector->data;

  var poly = blit3d_polygon_new();
  blit3d_polygon_load_data(poly, pts,  val.vector->count * val.vector->elem_size);
  blit3d_polygon_configure(poly, 2);

  return native_pointer(poly);
}

lisp_value blit_polygon (lisp_value val){
  if(val.type == LISP_CONS){
    blit3d_polygon * poly2[10];
    int i = 0;
    while(is_nil(val) == false){
      poly2[i] = val.cons->car.native_pointer;
      val = val.cons->cdr;
      i++;
    }
    blit3d_polygon_blit2(blit3d_current_context, poly2, i);
  
    return nil;
  }
  type_assert(val, LISP_NATIVE_POINTER);
  blit3d_polygon * poly = val.native_pointer;
  blit3d_polygon_blit2(blit3d_current_context, &poly, 1);
  return nil;
}

lisp_value delete_polygon (lisp_value val){
  type_assert(val, LISP_NATIVE_POINTER);
  blit3d_polygon * poly = val.native_pointer;
  blit3d_polygon_destroy(&poly);
  return nil;
}

lisp_value foxgl_create_window(lisp_value width, lisp_value height){
  type_assert(width, LISP_INTEGER);
  type_assert(height, LISP_INTEGER);
  gl_window * win = gl_window_open(width.integer, height.integer);
  
  
  return native_pointer(win);
}

lisp_value foxgl_get_events(){
  lisp_value evts = nil;
  gl_window_event events[20];
  while(true){
    size_t cnt = gl_get_events(events, array_count(events));
    if(cnt == 0)
      break;
    for(size_t i = 0; i < cnt; i++){
      gl_window_event evt = events[i];
      gl_event_known_event_types type = evt.type;
      lisp_value levt = nil;
      switch(type){
      case EVT_MOUSE_MOVE:
        levt = new_cons(get_symbol("mouse-move"), new_cons(integer(evt.mouse_move.x), integer(evt.mouse_move.y)));
        break;
      case EVT_MOUSE_LEAVE:
        levt = new_cons(get_symbol("mouse-leave"), nil);
        break;
      case EVT_MOUSE_ENTER:
        levt = new_cons(get_symbol("mouse-enter"), nil);
        break;
      case EVT_MOUSE_BTN_DOWN:
        levt = new_cons(get_symbol("mouse-button-down"),
                        new_cons(integer(evt.mouse_btn.button), nil));
        break;
      case EVT_MOUSE_BTN_UP:
        levt = new_cons(get_symbol("mouse-button-up"),
                        new_cons(integer(evt.mouse_btn.button), nil));
        break;
      case EVT_KEY_DOWN:
      case EVT_KEY_UP:
      case EVT_KEY_REPEAT:
        {
        char keystr[6] = {0};
        if(evt.key.codept != 0)
          codepoint_to_utf8(evt.key.codept, keystr, 5);
        
        
        lisp_value keyevt = nil;
        if(strlen(keystr) >0){
          levt = new_cons(get_symbol("char"), new_cons(get_symbol(keystr), nil));
          break;
        }
        else if(evt.key.key != -1){
          keyevt = new_cons(get_symbol("key"), new_cons(integer(evt.key.key), nil));
        }else if(evt.key.scancode != -1){
          keyevt = new_cons(get_symbol("scankey"), new_cons(integer(evt.key.scancode), nil));
        
        }
        
        levt = new_cons(get_symbol(type == EVT_KEY_DOWN ? "key-down" : (EVT_KEY_UP ? "key-up" : "key-repeat")), keyevt);
        }
        break;
      default:
        levt = new_cons(get_symbol("unknown"), new_cons(integer(type), nil));
      
      }
      levt = new_cons(get_symbol("event"), new_cons(get_symbol("timestamp"), new_cons(integer(evt.timestamp), levt)));
      evts = new_cons(levt, evts);
    }
  }
  return evts;
}

void foxgl_register(){
  lisp_register_native("blit-rect", 4, rect2);
}

//(define foxgl:create-framebuffer (load-wrap foxgl2 "lblit_create_framebuffer"))
//(define foxgl:bind-framebuffer (load-wrap foxgl2 "lblit_bind_framebuffer"))
//(define foxgl:destroy-framebuffer (load-wrap foxgl2 "lblit_destroy_framebuffer"))
//(define foxgl:framebuffer-texture (load-wrap foxgl2 "lblit_framebuffer_texture"))

lisp_value lblit_create_framebuffer (lisp_value width, lisp_value height){
  type_assert(width, LISP_INTEGER);
  type_assert(height, LISP_INTEGER);
  blit_framebuffer * b = lisp_malloc(sizeof(*b));
  b->width = width.integer;
  b->height = height.integer;
  b->channels = 4;
  b->mode = IMAGE_MODE_NONE;
 
  blit_create_framebuffer(b);
  return native_pointer(b);
}

lisp_value lblit_bind_framebuffer(lisp_value fb){
  //printf("Use fb\n");
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_use_framebuffer(fb.native_pointer);
  glClearColor(0,0,0,0);
  glClear(GL_COLOR_BUFFER_BIT);
  return nil;
}

lisp_value lblit_unbind_framebuffer(lisp_value fb){
  //printf("Unuse fb\n");
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_unuse_framebuffer(fb.native_pointer);
  return nil;
}

lisp_value lblit_destroy_framebuffer(lisp_value fb){
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_delete_framebuffer(fb.native_pointer);
  return nil;
}

lisp_value lblit_framebuffer_texture(lisp_value fb){
  type_assert(fb, LISP_NATIVE_POINTER);
  texture * tx = lisp_malloc(sizeof(*tx));
  *tx = blit_framebuffer_as_texture(fb.native_pointer);
  return native_pointer(tx);
}

//blit3d_bind_texture(blit3d_context * ctx, texture * tex){
lisp_value lblit_bind_texture(lisp_value tex){
  if(is_nil(tex)){
    blit3d_bind_texture(blit3d_current_context, NULL);
    return nil;
  }
  type_assert(tex, LISP_NATIVE_POINTER);
  
  blit3d_bind_texture(blit3d_current_context, tex.native_pointer);
  return nil;
}

lisp_value lblit_blit_text(lisp_value text, lisp_value matrix){
  type_assert(text, LISP_STRING);
  type_assert(matrix, LISP_VECTOR);
  mat4 * m1 = matrix.vector->data;

  blit3d_text(blit3d_current_context, mat4_identity(), *m1, text.string);

  return nil;
}

lisp_value lblit_blend(lisp_value blend){
  if(!is_nil(blend))
    glEnable(GL_BLEND);
  else
    glDisable(GL_BLEND);
}
