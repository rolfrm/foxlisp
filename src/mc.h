// From Bourke

typedef struct {
  f32 (*sdf)(void *userdata, vec3 pt, vec3 *color);
  f32 threshold;
  void *userdata;
} sdf_model;

int process_cube(sdf_model *model, vec3 pt, f32 size,
                 void (*f)(void *userdata, vec3 v1, vec3 v2, vec3 v3),
                 void *userdata);
