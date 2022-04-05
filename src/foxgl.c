#include <iron/full.h>
#include <iron/gl.h>
#include <iron/utf8.h>
#include "foxlisp.h"
#include <GL/gl.h>
#include <math.h>
#undef POP

lisp_value math_pow(lisp_value a, lisp_value b){
  type_assert(a, LISP_RATIONAL);
  type_assert(b, LISP_RATIONAL);
  return rational(pow(a.rational, b.rational));
}

lisp_value math_sqrt(lisp_value a){
  type_assert(a, LISP_RATIONAL);
  return rational(sqrt(a.rational));
}


lisp_value math_sqrtf(lisp_value a){
  return float32(sqrtf(lisp_float32(a).rational));
}

lisp_value math_random(lisp_value range){
  if(range.type == LISP_INTEGER){
    return integer(randu32((u32)range.integer));
  }
  if(range.type == LISP_RATIONAL || range.type == LISP_FLOAT32)
    return rational(randf64() * range.rational);
  return nil;
}

lisp_value foxgl_load_font(lisp_value str, lisp_value size){
  const char * fontfile_init = str.string;
  //printf("Load font: %s %i\n", str.string, size.integer);
  var fnt0 =  blit_load_font_file(fontfile_init,  size.integer);
  blit_set_current_font(fnt0);
  return nil;
}

lisp_value foxgl_load_texture_from_path(lisp_value str){
  image img2 = image_from_file(str.string);
  if(img2.source == NULL) return nil;

  texture duck_tex = texture_from_image(&img2);
  image_delete(&img2);
  texture * t = lisp_malloc(sizeof(duck_tex));
  *t = duck_tex;
  return (lisp_value){.type = LISP_NATIVE_POINTER, .native_pointer = t};
}

lisp_value math_mul_in_place(lisp_value target, lisp_value a, lisp_value b){
  type_assert(target, LISP_VECTOR);
  type_assert(a, LISP_VECTOR);
  type_assert(b, LISP_VECTOR);
  type_assert(a.vector->default_value, LISP_FLOAT32);
  type_assert(b.vector->default_value, LISP_FLOAT32);
  if(a.vector->count == 16){
	 mat4 * m1 = a.vector->data;
	 if(b.vector->count == 16){
		mat4 * m2 = b.vector->data;
		
		mat4 * m3 = target.vector->data;
      ASSERT(target.vector->count == 16);
      
      *m3 = mat4_mul(*m1, *m2);
		return nil;
	 }
    if(b.vector->count == 3){
      vec3 * m2 = b.vector->data;
      vec3 *m3 = ((vec3 *) target.vector->data);
      
      *m3 = mat4_mul_vec3(*m1, *m2);
      return nil;
    }
	 raise_string("Invalid number of rows and columns");	
  }else if(a.vector->count == 9){
    ASSERT(target.vector->count == 9);
    
	 mat3 * m1 = a.vector->data;
	 if(b.vector->count == 9){
		mat3 * m2 = b.vector->data;
		mat3 * m3 = target.vector->data;
		*m3 = mat3_mul(*m1, *m2);
		return nil;
	 }
  }

  raise_string("Invalid number of rows and columns");
  return a;
}


lisp_value math_mul(lisp_value a, lisp_value b){
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
    if(b.vector->count == 3){
      vec3 * m2 = b.vector->data;
      var c = vector_copy(b);
      ((vec3 *) c.vector->data)[0] = mat4_mul_vec3(*m1, *m2);
      return c;
    }
	 raise_string("Invalid number of rows and columns");	
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

  raise_string("Invalid number of rows and columns");
  return a;
}
lisp_value mat4_to_lisp (mat4 a);
lisp_value math_mat4_rotate(lisp_value x, lisp_value y, lisp_value z){
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

lisp_value math_mat4_perspective(lisp_value fov, lisp_value aspect, lisp_value near, lisp_value far){
  TYPE_ASSERT(fov, LISP_RATIONAL);
  TYPE_ASSERT(aspect, LISP_RATIONAL);
  TYPE_ASSERT(near, LISP_RATIONAL);
  TYPE_ASSERT(far, LISP_RATIONAL);
  mat4 p = mat4_perspective(fov.rational, aspect.rational, near.rational, far.rational);
  return mat4_to_lisp(p);
}
           
lisp_value math_mat4_orthographic(lisp_value w, lisp_value h, lisp_value z){
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
  }
  raise_string("Invalid number of rows and columns");
  return mat4_identity();
}


lisp_value mat4_to_lisp (mat4 a){
  lisp_value vec = make_vector(integer(16), (lisp_value){.type = LISP_FLOAT32, .rational = 0.0f});
  mat4 * v = vec.vector->data;
  *v = a;
  return vec;
}

lisp_value math_mat4_print(lisp_value a){
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

  raise_string("Invalid number of rows and columns");
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

lisp_value foxgl_create_window(lisp_value w, lisp_value h){
  TYPE_ASSERT(w, LISP_INTEGER);
  TYPE_ASSERT(h, LISP_INTEGER);
  return native_pointer(gl_window_open(w.integer, h.integer));
}
lisp_value foxgl_make_current(lisp_value win){
  TYPE_ASSERT(win, LISP_NATIVE_POINTER);
  gl_window_make_current(win.native_pointer);
  return nil;
}
lisp_value foxgl_set_title(lisp_value win, lisp_value title){
  TYPE_ASSERT(win, LISP_NATIVE_POINTER);
  gl_window_set_title(win.native_pointer, title.string);
  return nil;
}

lisp_value foxgl_swap(lisp_value win){
  TYPE_ASSERT(win, LISP_NATIVE_POINTER);
  gl_window_swap(win.native_pointer);
  return nil;
}

lisp_value foxgl_poll_events(){
  gl_window_poll_events();
  return nil;
}

blit3d_context * blit3d_current_context = NULL;

lisp_value foxgl_color(lisp_value color){
  vec4 col = lisp_to_color(color);
  if(blit3d_current_context != NULL)
	 blit3d_color(blit3d_current_context, col);
  return nil;
}

lisp_value foxgl_transform(lisp_value tform1){
  mat4 tform = lisp_to_mat4(tform1);
  if(blit3d_current_context != NULL)
	 blit3d_view(blit3d_current_context, tform);
  return nil;
}

lisp_value foxgl_init(){
  if(blit3d_current_context == NULL)
	 blit3d_current_context = blit3d_context_new();
  blit3d_context_load(blit3d_current_context);
  //glDisable(GL_BLEND);
  return nil;
}

lisp_value foxgl_square2(){
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

lisp_value load_polygon (lisp_value val, lisp_value dim){
  if(blit3d_current_context == NULL) return nil;
  type_assert(val, LISP_VECTOR);
  type_assert(val.vector->default_value, LISP_FLOAT32);
  int dimensions = 2;
  if(dim.type != LISP_NIL){
    type_assert(dim, LISP_INTEGER);
    dimensions = dim.integer;
  }
  var pts = (float *) val.vector->data;

  var poly = blit3d_polygon_new();
  blit3d_polygon_load_data(poly, pts,  val.vector->count * val.vector->elem_size);
  blit3d_polygon_configure(poly, dimensions);

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

lisp_value foxgl_create_framebuffer (lisp_value width, lisp_value height){
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

lisp_value foxgl_bind_framebuffer(lisp_value fb){
  //printf("Use fb\n");
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_use_framebuffer(fb.native_pointer);
  glClearColor(0,0,0,0);
  glClear(GL_COLOR_BUFFER_BIT);
  return nil;
}

lisp_value foxgl_clear(){
  glClearColor(0,0,0,0);
  glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
  return nil;
}

lisp_value foxgl_unbind_framebuffer(lisp_value fb){
  //printf("Unuse fb\n");
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_unuse_framebuffer(fb.native_pointer);
  return nil;
}

lisp_value foxgl_destroy_framebuffer(lisp_value fb){
  type_assert(fb, LISP_NATIVE_POINTER);
  blit_delete_framebuffer(fb.native_pointer);
  return nil;
}

lisp_value foxgl_framebuffer_texture(lisp_value fb){
  type_assert(fb, LISP_NATIVE_POINTER);
  texture * tx = lisp_malloc(sizeof(*tx));
  *tx = blit_framebuffer_as_texture(fb.native_pointer);
  return native_pointer(tx);
}

//blit3d_bind_texture(blit3d_context * ctx, texture * tex){
lisp_value foxgl_bind_texture(lisp_value tex){
  if(is_nil(tex)){
    blit3d_bind_texture(blit3d_current_context, NULL);
    return nil;
  }
  type_assert(tex, LISP_NATIVE_POINTER);
  
  blit3d_bind_texture(blit3d_current_context, tex.native_pointer);
  return nil;
}

lisp_value foxgl_blit_text(lisp_value text, lisp_value matrix){
  type_assert(text, LISP_STRING);
  type_assert(matrix, LISP_VECTOR);
  mat4 * m1 = matrix.vector->data;

  blit3d_text(blit3d_current_context, mat4_identity(), *m1, text.string);

  return nil;
}

lisp_value foxgl_blend(lisp_value blend){
  if(!is_nil(blend))
    glEnable(GL_BLEND);
  else
    glDisable(GL_BLEND);
  return nil;
}


lisp_value foxgl_depth(lisp_value blend){
  if(!is_nil(blend))
    glEnable(GL_DEPTH_TEST);
  else
    glDisable(GL_DEPTH_TEST);
  return nil;
}


lisp_value foxgl_key_down(lisp_value window, lisp_value keyid){
  if(gl_window_get_key_state(window.native_pointer, keyid.integer))
    return t;
  return nil;
}

lisp_value foxgl_timestamp(){
  return integer(timestamp());
}


void lrn(const char * l, int args, void * f){
  lisp_register_native(l, args, f);
}
void tcp_register();
void foxal_register();
void foxgl_register(){
  lrn("math:pow", 2, math_pow);
  lrn("math:sqrt", 1, math_sqrt);
  lrn("math:sqrtf", 1, math_sqrtf);
  lrn("math:random", 1, math_random);
  lrn("foxgl:load-font", 2, foxgl_load_font);
  lrn("foxgl:load-texture-from-path", 1, foxgl_load_texture_from_path);
  lrn("foxgl:create-window", 2, foxgl_create_window);
  lrn("foxgl:make-current", 1, foxgl_make_current);
  lrn("foxgl:set-title", 2, foxgl_set_title);
  lrn("foxgl:swap", 1, foxgl_swap);
  lrn("foxgl:poll-events", 0, foxgl_poll_events);
  
  lrn("math:*", 2, math_mul);
  lrn("math:*!", 3, math_mul_in_place);
  lrn("mat4:rotate", 3, math_mat4_rotate);
  lrn("mat4:perspective", 4, math_mat4_perspective);
  lrn("mat4:orthographic", 3, math_mat4_orthographic);
  lrn("mat4:print", 1, math_mat4_print);
  lrn("foxgl:timestamp", 0, timestamp);
  lrn("foxgl:clear", 0, foxgl_clear);
  lrn("foxgl:get-events", 0, foxgl_get_events);
  
  lrn("foxgl:color", 1, foxgl_color);
  lrn("foxgl:transform", 1, foxgl_transform);
  lrn("foxgl:init", 0, foxgl_init);
  lrn("foxgl:quad", 1, foxgl_square2);
  lrn("foxgl:load-polygon", 2, load_polygon);
  lrn("foxgl:blit-polygon", 1, blit_polygon);
  lrn("foxgl:delete-polygon", 1, delete_polygon);
  
  lrn("foxgl:create-framebuffer", 2, foxgl_create_framebuffer);
  lrn("foxgl:bind-framebuffer", 1, foxgl_bind_framebuffer);
  lrn("foxgl:unbind-framebuffer", 1, foxgl_unbind_framebuffer);
  lrn("foxgl:destroy-framebuffer", 1, foxgl_destroy_framebuffer);

  lrn("foxgl:frame-buffer-texture", 1, foxgl_framebuffer_texture);
  lrn("foxgl:bind-texture", 1, foxgl_bind_texture);
  lrn("foxgl:blit-text", 2, foxgl_blit_text);
  lrn("foxgl:blend", 1, foxgl_blend);
  lrn("foxgl:depth", 1, foxgl_depth);
  lrn("foxgl:key-down?", 2, foxgl_key_down);
  tcp_register();
  foxal_register();
}
