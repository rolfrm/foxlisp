#include <iron/full.h>
#include "foxlisp.h"
#include <iron/audio.h>
static audio_context * audio;

static void init(){
  
  if(audio == NULL){
    audio = audio_initialize(44100);
    audio_context_make_current(audio);
  }
}

lisp_value foxal_load_sample(lisp_value samples){
  type_assert(samples, LISP_VECTOR);
  elem_type_assert(samples, LISP_FLOAT32);
  init();
  audio_sample samp = audio_load_samplef(audio, samples.vector->data, samples.vector->count);
  
  return new_cons(get_symbol("sample"), integer(samp.sample_id));
}

lisp_value foxal_play_sample(lisp_value samples){
  type_assert(samples, LISP_CONS);
  audio_sample samp = {.sample_id = cdr(samples).integer};
  audio_play_sample(audio, samp);
  return t;
}

lisp_value foxal_update(){
  if(audio != NULL)
    audio_update_streams(audio);
  return nil;
}

void lrn(const char * l, int args, void * f);

void foxal_register(){
  lrn("audio:update", 0, foxal_update);
  lrn("audio:play-sample", 1, foxal_play_sample);
  lrn("audio:load-sample", 1, foxal_load_sample);
}
