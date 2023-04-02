#include <iron/full.h>
#include <microio.h>
#include "foxlisp.h"
#include <iron/audio.h>
static audio_context *audio;

static void init() {

  if (audio == NULL) {
    audio = audio_initialize(44100);
    audio_context_make_current(audio);
  }
}

lisp_value foxal_load_sample(lisp_value samples) {
  type_assert(samples, LISP_VECTOR);
  elem_type_assert(samples, LISP_FLOAT32);
  init();

  f32 *data = samples.vector->data;
  size_t count = samples.vector->count;
  audio_sample samp = audio_load_samplef(audio, data, (int)count);

  return new_cons(get_symbol("sample"), integer(samp.sample_id));
}

lisp_value foxal_play_sample(lisp_value samples) {
  type_assert(samples, LISP_CONS);
  audio_sample samp = {.sample_id = cdr(samples).integer};
  audio_play_sample(audio, samp);
  return t;
}

lisp_value foxal_new_source() {
  var src = audio_new_source();
  return new_cons(get_symbol("source"), integer_lisp_value(src));
}

lisp_value foxal_source_play(lisp_value source) {
  var sourceId = (u32)lisp_value_integer(cdr(source));
  audio_source_play(sourceId);
  return t;
}

lisp_value foxal_source_queue(lisp_value source, lisp_value sample) {
  var sourceId = (u32)lisp_value_integer(cdr(source));
  var sampleId = (u32)lisp_value_integer(cdr(sample));
  audio_source_queue(sourceId, sampleId);
  return t;
}

lisp_value foxal_source_buffer_count(lisp_value source) {
  return integer_lisp_value(
      audio_source_count(lisp_value_integer(cdr(source))));
}

lisp_value foxal_source_update(lisp_value source) {
  int v = audio_update_source(lisp_value_integer(cdr(source)));
  if (v == -1)
    return nil;
  return new_cons(get_symbol("buffer"), integer_lisp_value(v));
}

lisp_value foxal_fill_buffer(lisp_value buffer, lisp_value samples) {
  var i = lisp_value_integer(cdr(buffer));
  f32 *data = samples.vector->data;
  size_t count = samples.vector->count;
  audio_fill_bufferf((u32)i, data, count);
  return t;
}

lisp_value foxal_sin(lisp_value buffer, lisp_value phase, lisp_value freq) {
  f32 *data = buffer.vector->data;
  size_t count = buffer.vector->count;
  f32 p = lisp_value_as_rational(phase);
  f32 f = lisp_value_as_rational(freq);
  for (size_t i = 0; i < count; i++) {
    data[i] = sinf(p);
    p += f;
  }

  return rational_lisp_value(p);
}

lisp_value foxal_update() {
  if (audio != NULL)
    audio_update_streams(audio);
  return nil;
}

void lrn(const char *l, int args, void *f);

void foxal_register() {
  lrn("audio:update", 0, foxal_update);
  lrn("audio:play-sample", 1, foxal_play_sample);
  lrn("audio:load-sample", 1, foxal_load_sample);
  lrn("audio:new-source", 0, foxal_new_source);
  lrn("audio:source-play", 1, foxal_source_play);
  lrn("audio:source-queue", 2, foxal_source_queue);
  lrn("audio:source-buffer-count", 1, foxal_source_buffer_count);
  lrn("audio:source-update", 1, foxal_source_update);
  lrn("audio:fill-buffer", 2, foxal_fill_buffer);
  lrn("vec:sin", 3, foxal_sin);
}
