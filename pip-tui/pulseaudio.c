#include <libguile.h>
#include <pulse/pulseaudio.h>
#include <stdbool.h>

#define MICROSECONDS_PER_MILLISECOND (1000)

#define AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS (100)

/** Samples per seconds of the audio engine */
#define AUDIO_SAMPLE_RATE_IN_HZ (44100u)

/** The size of the audio buffer.  Consequently the maximum note length. */
#define AUDIO_BUFFER_DURATION_IN_MILLISECONDS (5000u)

/** Number of samples in the audio buffer */
#define AUDIO_BUFFER_SIZE \
  ((AUDIO_BUFFER_DURATION_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000u)

/** Number of independent channels */
#define AUDIO_CHANNEL_COUNT 16

/** Maximum amplitude of PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MAX 32767
#define AUDIO_CHANNEL_AMPLITUDE_MAX_F ((double)(AUDIO_CHANNEL_AMPLITUDE_MAX))
/** Minimum amplitude of a PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MIN (-32767)


SCM pulse_bv;
SCM scm_pulse_bv;
SCM pulse_stream_read_proc;

void pulse_initialize_audio_step_2(pa_context *context);


static bool
is_valid_pa_context_state_t (pa_context_state_t c)
{
  return (c == PA_CONTEXT_UNCONNECTED
	    || c == PA_CONTEXT_CONNECTING
	    || c == PA_CONTEXT_AUTHORIZING
	    || c == PA_CONTEXT_SETTING_NAME
	    || c == PA_CONTEXT_READY
	    || c == PA_CONTEXT_FAILED
	  || c == PA_CONTEXT_TERMINATED);
}

void
xpa_context_connect_to_default_server (pa_context *c)
{
  assert (c != NULL);

  int ret;
  ret = pa_context_connect (c,
			    (const char *) NULL,
			    PA_CONTEXT_NOFLAGS,
			    (const pa_spawn_api *) NULL);
  /* if (ret != 0) */
  /*   g_critical ("pa_context_connect failed"); */
}

pa_context_state_t
xpa_context_get_state (pa_context *c)
{
  pa_context_state_t s;
  assert (c != NULL);
  s = pa_context_get_state (c);
  /* g_return_val_if_fail (is_valid_pa_context_state_t (s), s); */
  return s;
}

pa_context *
xpa_context_new_with_proplist (pa_mainloop_api *mainloop,
			       const char *name,
			       pa_proplist *proplist)
{
  /* g_return_val_if_fail (mainloop != NULL, NULL); */
  /* g_return_val_if_fail (name != NULL, NULL); */
  /* g_return_val_if_fail (proplist != NULL, NULL); */

  pa_context *c;
  c = pa_context_new_with_proplist (mainloop, name, proplist);
  if (c == NULL)
    abort ();
  //g_critical ("pa_context_new_with_proplist returned NULL");
  return c;
}

void
xpa_context_set_state_callback (pa_context *c, pa_context_notify_cb_t cb, void *userdata)
{
  //g_return_if_fail (c != NULL);
  //g_return_if_fail (cb != NULL);
  pa_context_set_state_callback (c, cb, userdata);
}

pa_mainloop_api *
xpa_mainloop_get_api (pa_mainloop *m)
{
  pa_mainloop_api *api;
  //g_return_val_if_fail (m != NULL, NULL);
  api = pa_mainloop_get_api (m);
  if (api == NULL)
    abort(); // g_critical ("pa_mainloop_get_api returned NULL");
  return api;
}

void
xpa_mainloop_blocking_iterate (pa_mainloop * m)
{
  int ret;
  // g_return_if_fail (m != NULL);
  ret = pa_mainloop_iterate (m, 1, NULL);
  if (ret < 0)
    abort ();
  //g_critical ("pa_mainloop_iterate did not succeed");
}

int
xpa_mainloop_nonblocking_iterate (pa_mainloop * m)
{
  int ret;
  // g_return_val_if_fail (m != NULL, -1);
  ret = pa_mainloop_iterate (m, 0, NULL);
  if (ret < 0)
    abort();
  //g_critical ("pa_mainloop_iterate did not succeed");
  return ret;
}


void
xpa_mainloop_free (pa_mainloop *m)
{
  //g_return_if_fail (m != NULL);
  pa_mainloop_free (m);
}

pa_mainloop *
xpa_mainloop_new (void)
{
  pa_mainloop *ml = pa_mainloop_new ();
  if (ml == NULL)
    abort();
  //g_critical ("pa_mainloop_new returned NULL");
  return ml;
}

void
xpa_proplist_free (pa_proplist *p)
{
  //g_return_if_fail (p != NULL);
  pa_proplist_free (p);
}

pa_proplist *
xpa_proplist_new (void)
{
  pa_proplist *p;
  p = pa_proplist_new ();
  if (p == NULL)
    abort();
  //g_critical ("pa_proplist_new returned NULL");
  return p;
}

void
xpa_proplist_sets (pa_proplist *p, const char *key, const char *value)
{
  int ret;
  ret = pa_proplist_sets (p, key, value);
  if (ret != 0)
    abort ();
  //g_critical ("pa_proplist_sets failed");
}

void xpa_stream_connect_playback_to_default_device (pa_stream *s, pa_context *c,
						    const pa_buffer_attr *attr,
						    pa_stream_flags_t flags)
{
  int ret;
  //g_return_if_fail (s != NULL);
  ret = pa_stream_connect_playback (s, NULL, attr, flags, NULL, NULL);
  if (ret != 0)
    abort();
  // g_critical ("pa_stream_connect_playback failed: %s",		pa_strerror (pa_context_errno(c)));
}

pa_stream *
xpa_stream_new_with_proplist (pa_context *c, const char *name, const pa_sample_spec *ss,
			      const pa_channel_map *map, pa_proplist *p)
{
  pa_stream *s;
  /* g_return_val_if_fail (c != NULL, NULL); */
  /* g_return_val_if_fail (name != NULL, NULL); */
  /* g_return_val_if_fail (ss != NULL, NULL); */
  /* g_return_val_if_fail (map != NULL, NULL); */
  /* g_return_val_if_fail (p != NULL, NULL); */
  s = pa_stream_new_with_proplist (c, name, ss, map, p);
  /* if (s == NULL) */
  /*   g_critical ("pa_stream_new_with_proplist returned NULL: %s", */
  /* 		pa_strerror (pa_context_errno(c))); */
  return s;
}

void
xpa_stream_set_started_callback (pa_stream *p, pa_stream_notify_cb_t cb, void *userdata)
{
  /* g_return_if_fail (p != NULL); */
  /* g_return_if_fail (cb != NULL); */
  pa_stream_set_started_callback (p, cb, userdata);
}

void
xpa_stream_set_write_callback (pa_stream *p, pa_stream_request_cb_t cb, void *userdata)
{
  /* g_return_if_fail (p != NULL); */
  /* g_return_if_fail (cb != NULL); */
  pa_stream_set_write_callback (p, cb, userdata);
}

void
xpa_stream_write (pa_stream *p, const void *data, size_t nbytes)
{
  /* g_return_if_fail (p != NULL); */
  /* g_return_if_fail (data != NULL); */
  /* g_return_if_fail (nbytes > 0); */

  int ret;
  ret = pa_stream_write (p, data, nbytes, NULL, 0, PA_SEEK_RELATIVE);
  /* if (ret != 0) */
  /*   g_critical ("pa_stream_write failed"); */
}


#define PIP_PROP_MEDIA_ROLE "game"
#define PIP_PROP_APPLICATION_ID "com.lonelycactus.projectpip"
#define PIP_PROP_APPLICATION_NAME "ProjectPip"

typedef struct pulse_priv_tag {
  pa_context_state_t state;
  pa_context *context;
  pa_mainloop *loop;
  bool finalize;
  unsigned samples_written;
} pulse_priv_t;
static pulse_priv_t pulse;

static void cb_audio_context_state(pa_context *c, void *userdata);
static void cb_audio_stream_started(pa_stream *p,
				    void *userdata);
static void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata);

/* This callback gets called when our context changes state.  We
 * really only care about when it's ready or if it has failed. */
static void cb_audio_context_state(pa_context *c, void *userdata)
{
  pulse.state = (int) xpa_context_get_state(c);
  switch(pulse.state)
    {
    case PA_CONTEXT_UNCONNECTED:
      // g_debug("PulseAudio set context state to UNCONNECTED");
      break;
    case PA_CONTEXT_CONNECTING:
      // g_debug("PulseAudio set context state to CONNECTING");
      break;
    case PA_CONTEXT_AUTHORIZING:
      // g_debug("PulseAudio set context state to AUTHORIZING");
      break;
    case PA_CONTEXT_SETTING_NAME:
      // g_debug("PulseAudio set context state to SETTING NAME");
      break;
    case PA_CONTEXT_FAILED:
      // g_debug("PulseAudio set context state to FAILED");
      break;
    case PA_CONTEXT_TERMINATED:
      // g_debug("PulseAudio set context state to TERMINATED");
      break;
    case PA_CONTEXT_READY:
      // g_debug("PulseAudio set context state to READY");
      pulse_initialize_audio_step_2 (c);
      break;
    default:
      // g_debug("PulseAudio set context state to %d", pulse.state);
      break;
    }
}

/* This callback is called when the server starts playback after an
 * underrun or on initial startup. */
static void cb_audio_stream_started(pa_stream *p, void *userdata)
{
  // g_debug("PulseAudio started playback");
}


/* This is called when new data may be written to the stream.  If we
  have data, we can ship it, otherwise we just note that the stream is
  waiting.  */
static void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata)
{
  // g_return_if_fail (p != NULL);

  const pa_timing_info *timing = pa_stream_get_timing_info (p);
  unsigned n = nbytes / sizeof (uint16_t);

  /* if (timing != NULL) */
  /*     g_debug("Pulseaudio requests %d samples, %u microseconds to live", */
  /*             n, timing->transport_usec + timing->sink_usec); */
  pa_stream_update_timing_info (p, NULL, NULL);
  if (n > AUDIO_BUFFER_SIZE)
    {
      /* g_warning ("Pulseaudio buffer read overflow %u > %u", */
      /* 		 n, AUDIO_BUFFER_SIZE); */
      n = AUDIO_BUFFER_SIZE;
      nbytes = n * sizeof (uint16_t);
    }
  //  SCM func = scm_variable_ref (pulse_stream_read_proc);
  if (scm_is_true (scm_procedure_p (pulse_stream_read_proc)))
    scm_call_1 (pulse_stream_read_proc, scm_from_uint (n));
  xpa_stream_write(p, SCM_BYTEVECTOR_CONTENTS(pulse_bv), nbytes);
  memmove(SCM_BYTEVECTOR_CONTENTS(pulse_bv),
          SCM_BYTEVECTOR_CONTENTS(pulse_bv) + nbytes,
          AUDIO_BUFFER_SIZE - nbytes);
  memset(SCM_BYTEVECTOR_CONTENTS(pulse_bv) + (AUDIO_BUFFER_SIZE - nbytes),
         0,
         nbytes);
  pulse.samples_written += n;
}

void pulse_initialize_audio_step_1()
{
  pa_mainloop_api *vtable = NULL;
  pa_proplist *main_proplist = NULL;
  pa_channel_map channel_map;

  pulse.loop = pa_mainloop_new ();
  vtable = xpa_mainloop_get_api(pulse.loop);

  /* PROPLIST: Only the PA_PROP_MEDIA_ROLE is important.  */
  main_proplist = xpa_proplist_new();
  xpa_proplist_sets(main_proplist, PA_PROP_MEDIA_ROLE, PIP_PROP_MEDIA_ROLE);
  xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_ID,
		    PIP_PROP_APPLICATION_ID);
  xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_NAME,
		    PIP_PROP_APPLICATION_NAME);

  /* A context is the basic object for a connection to a PulseAudio
       server. It multiplexes commands, data streams and events
       through a single channel.  */
  pulse.context = xpa_context_new_with_proplist(vtable,
						PIP_PROP_APPLICATION_NAME,
						main_proplist);
  xpa_context_set_state_callback(pulse.context,
				 cb_audio_context_state, NULL);

  pulse.state = PA_CONTEXT_UNCONNECTED;

  /* Connect the context */
  xpa_context_connect_to_default_server(pulse.context);

}

void pulse_initialize_audio_step_2(pa_context *context)
{
  pa_proplist *stream_proplist;
  pa_sample_spec sample_specification;
  pa_channel_map channel_map;
  pa_buffer_attr buffer_attributes;
  pa_stream *stream;

  /* Now we need to add our mono audio channel to the connection */

  /* SAMPLE_SPEC:  we describe the data going into our channel */
  sample_specification.format = PA_SAMPLE_S16NE;
  sample_specification.rate = AUDIO_SAMPLE_RATE_IN_HZ;
  sample_specification.channels = 1;
  assert(pa_sample_spec_valid(&sample_specification));

  /* BUFFER_ATTRIBUTES: Here we set the buffering behavior of the
   * audio.  We want low latency.

       One wiki suggests that to set a specific latency, 
       1. use pa_usec_to_bytes(&ss, ...) to convert the latency from a time unit to bytes
       2. use the PA_STREAM_ADJUST_LATENCY flag
       3. set pa_buffer_attr::tlength to latency in samples
       4. set rest of pa_buffer_attr to -1

       http://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/LatencyControl
  */

  buffer_attributes.tlength = pa_usec_to_bytes (AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS * MICROSECONDS_PER_MILLISECOND,
						&sample_specification);
  buffer_attributes.maxlength = -1; /* -1 == default */
  buffer_attributes.prebuf = -1;
  buffer_attributes.minreq = -1;
  buffer_attributes.fragsize = -1;

  /* PROPERTY_LIST: Then we set up a structure to hold PulseAudio
   * properties for this. */
  stream_proplist = xpa_proplist_new();
  xpa_proplist_sets(stream_proplist, PA_PROP_MEDIA_NAME, "mono channel");
  xpa_proplist_sets(stream_proplist, PA_PROP_MEDIA_ROLE, "game");

  /* CHANNEL_MAP: Then we say which speakers we're using.  The game
     is mono, so - that makes it simple. */

  pa_channel_map_init_extend(&channel_map, 1, PA_CHANNEL_MAP_DEFAULT);

  if (!pa_channel_map_compatible(&channel_map, &sample_specification))
    abort();
    // g_error("Channel map doesn't match sample specification");

  {
    char tss[100], tcm[100];
    /* g_debug("Opening a stream with sample specification '%s' and channel map '%s'.", */
    /* 	    pa_sample_spec_snprint(tss, sizeof(tss), &sample_specification), */
    /* 	    pa_channel_map_snprint(tcm, sizeof(tcm), &channel_map)); */
  }

  /* STREAM: Group everything together as a stream */
  // g_assert (pulse.context == context);
  stream = xpa_stream_new_with_proplist(pulse.context,
					"mono channel", /* Stream name */
					&sample_specification,
					&channel_map,
					stream_proplist);
  xpa_proplist_free (stream_proplist);

  xpa_stream_set_started_callback(stream, cb_audio_stream_started, NULL);
  xpa_stream_set_write_callback(stream, cb_audio_stream_write, NULL);

  pulse.samples_written = 0U;

  /* Connect the stream to the audio loop */
  xpa_stream_connect_playback_to_default_device (stream, context,
						 &buffer_attributes,
						 PA_STREAM_ADJUST_LATENCY);
  /* Finally done! */
  // g_debug("PulseAudio initialization complete");
}

/* This finalizer is called if we are shutting down cleanly */
void pulse_finalize_audio()
{
  // xpa_mainloop_free(pulse.loop);
  pa_mainloop_free(pulse.loop);
  pulse.finalize = true;
  // g_debug("PulseAudio finalization complete");
}

SCM
pa_iterate (void)
{
  xpa_mainloop_blocking_iterate (pulse.loop);
  return SCM_UNSPECIFIED;
}

void
pip_pulseaudio_init ()
{
  pulse_initialize_audio_step_1();
  pulse_bv = scm_c_make_bytevector (AUDIO_BUFFER_SIZE * sizeof(int16_t));
  scm_bytevector_fill_x (pulse_bv, scm_from_uint(0));
  scm_pulse_bv =
    scm_permanent_object (scm_c_define ("%audio-buffer", pulse_bv));
  pulse_stream_read_proc =
    scm_permanent_object (scm_c_define ("%audio-write-cb", SCM_BOOL_F));
  pulse_stream_read_proc = SCM_BOOL_F;
  scm_c_define_gsubr ("%pa-iterate", 0, 0, 0, pa_iterate);
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/

