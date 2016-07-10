#include <libguile.h>
#include <pulse/pulseaudio.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>

/* This is a simple audio model and control, using Pulseaudio primitives.
 *
 * There are 16 channel buffers of signed 16-bit data, with a 5 second
 * buffer for each.  When Pulseaudio asks for more samples, the
 * channels are mixed into a single channel, clipped if necessary, and
 * then shipped to pulseaudio.  The channels are then left-shifted
 * with zeroes added at the end.
 *
 * To make music work, you need to know the time at which a sample in
 * a channel buffer will be played.
 */

#define MICROSECONDS_PER_MILLISECOND (1000)

#define AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS (100)

/** Number of independent channels */
#define AUDIO_CHANNEL_COUNT 16

/** Samples per seconds of the audio engine */
#define AUDIO_SAMPLE_RATE_IN_HZ (44100u)

/** The size of the audio buffer.  Consequently the maximum note length. */
#define AUDIO_BUFFER_DURATION_IN_MILLISECONDS (5000u)

/** Number of samples in the audio buffer */
#define AUDIO_BUFFER_SIZE \
  ((AUDIO_BUFFER_DURATION_IN_MILLISECONDS * AUDIO_SAMPLE_RATE_IN_HZ) / 1000u)

/** Maximum amplitude of PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MAX 32767
#define AUDIO_CHANNEL_AMPLITUDE_MAX_F ((double)(AUDIO_CHANNEL_AMPLITUDE_MAX))
/** Minimum amplitude of a PCM waveform per channel */
#define AUDIO_CHANNEL_AMPLITUDE_MIN (-32767)

/* Holds the information of the audio model */
SCM am_channel_bytevectors[AUDIO_CHANNEL_COUNT];

/* A vector that holds the bytevectors */
SCM am_channel_bytevector_store;

/* Cache of pointers that point directly to the data in the bytevectors. */
static int16_t* am_channels[AUDIO_CHANNEL_COUNT];

/* Workspace to compute the sum of the channels. */
static double am_working[AUDIO_BUFFER_SIZE];
static int16_t am_sum[AUDIO_BUFFER_SIZE];
static double am_update_time = 0.0;

static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t *buffer, size_t *length);
static void
update_sum();

double audio_model_get_update_time()
{
    return am_update_time;
}

void audio_model_add_tone(int channel, double start_time,
                          double D_attack, double D_decay, double D_sustain,
                          double D_release, double F_initial, double F_attack,
                          double F_sustain, double F_release, double A_attack,
                          double A_sustain, double duty, int noise,
                          int waveform)
{
    size_t length;

    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    double now = (double) tp.tv_sec + (double) tp.tv_nsec * 1e-9;

    double time_since_last_update = now - am_update_time;
    double delta_t;

    if (start_time == 0.0)
        delta_t = 0.0;
    else
        delta_t = start_time - am_update_time;

    int delta_t_in_samples = delta_t * AUDIO_SAMPLE_RATE_IN_HZ;
    length = AUDIO_BUFFER_SIZE - delta_t_in_samples;

    generate_tone_data(D_attack, D_decay, D_sustain, D_release,
                       F_initial, F_attack, F_sustain, F_release,
                       A_attack, A_sustain,
                       duty, noise, waveform,
                       &(am_channels[channel][delta_t_in_samples]), &length);

}

// Remove N samples from the model
void audio_model_dequeue(unsigned n)
{
    int c;
    size_t samples_moved = AUDIO_BUFFER_SIZE - n;
    size_t bytes_moved = sizeof(int16_t) * samples_moved;
    size_t bytes_remaining = sizeof(int16_t) * n;

    for (c = 0; c < AUDIO_CHANNEL_COUNT; c ++)
    {
        memmove (&(am_channels[c][0]), &(am_channels[c][n]), bytes_moved);
        memset (&(am_channels[c][samples_moved]), 0, bytes_remaining);
    }
}

static void
generate_tone_data(double D_attack, double D_decay, double D_sustain,
                   double D_release, double F_initial, double F_attack,
                   double F_sustain, double F_release, double A_attack,
                   double A_sustain, double duty, _Bool noise, int waveform,
                   int16_t *buffer, size_t *length)
{
    /* D = duration in sec
       F = frequency in Hz
       A = amplitude, from 0.0 to 1.0 */
    double t, t_start, amplitude, frequency, period;
    double duration;
    size_t i;
    int first;
    int level_a, level_b;
    static int count = 0;
    count ++;

    duration = D_attack + D_decay + D_sustain + D_release;
    size_t tone_length = (size_t) ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    if (*length > tone_length)
        tone_length = *length;

    t = 0.0;
    t_start = 0.0;
    i = 0;
    period = 0.0;
    first = true;
    while (i < tone_length)
    {
        if(first || t - t_start >= period)
        {
            if (first)
                first = false;
            else
                while (t - t_start >= period)
                    t_start += period;

            if (t < D_attack)
            {
                amplitude = (A_attack / D_attack) * t;
                frequency = ((F_attack - F_initial) / D_attack) *  t + F_initial;
            }
            else if (t < D_attack + D_decay)
            {
                amplitude = ((A_sustain - A_attack) / D_decay) * (t - D_attack) + A_attack;
                frequency = ((F_sustain - F_attack) / D_decay) * (t - D_attack) + F_attack;
            }
            else if (t < D_attack + D_decay + D_sustain)
            {
                amplitude = A_sustain;
                frequency = F_sustain;
            }
            else if (t < D_attack + D_decay + D_sustain + D_release)
            {
                amplitude = (-A_sustain / D_release) * (t - D_attack - D_decay - D_sustain) + A_sustain;
                frequency = ((F_release - F_sustain) / D_release) * (t - D_attack - D_decay - D_sustain) + F_sustain;
            }
            else
            {
                amplitude = 0;
                frequency = F_release;
            }
            period = 1.0 / frequency;
            if (noise)
            {
                if(random() % 2 == 0)
                    level_a = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                else
                    level_a = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                if(random() % 2 == 0)
                    level_b = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                else
                    level_b = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
            }
            else
            {
                level_a = AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
                level_b = -AUDIO_CHANNEL_AMPLITUDE_MAX_F * amplitude;
            }

        }
        if (t - t_start < period * duty)
        {
            if(waveform == 0)
                buffer[i] = level_a;
            else if (waveform == 1)
                buffer[i] = level_a * sin(M_PI * (t - t_start) / (period * duty));
        }
        else if (t - t_start < period)
        {
            if(waveform == 0)
                buffer[i] = level_b;
            else if (waveform == 1)
                buffer[i] = level_b * sin(M_PI * ((t - t_start) - period * duty) / (period * (1.0 - duty)));
        }
        i ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
#if 1
    {
        if (count % 100 == 1) {
            FILE *fp;
            if(noise)
                fp = fopen("noise.txt", "wt");
            else
                fp = fopen("wave.txt", "wt");
            for(unsigned i2 = 0; i2 < *length; i2++)
                fprintf(fp, "%u %d\n", i2, (int)buffer[i2]);
            fclose(fp);
        }
    }
#endif
}

// FIXME: this is pretty lazy.
static void
update_sum(int n)
{
    int i, j;
    float min, max;
    memset (am_working, 0, n * sizeof(double));
    for (j = 0; j < AUDIO_CHANNEL_COUNT; j ++)
    {
        for (i = 0; i < n; i ++) {
            am_working[i] += (double) am_channels[j][i];
        }
    }

    /* I used 0x7ffc instead of INT_MAX so I can avoid thinking about
     * overflow. */
    min = (double) -0x7ffc;
    max = (double) 0x7ffc;
    for (i = 0; i < n; i ++) {
        if (am_working[i] > max)
            am_working[i] = max;
        if (am_working[i] < min)
            am_working[i] = min;
    }

    for (i = 0; i < n; i ++) {
        am_sum[i] = (int16_t) am_working[i];
    }
}

SCM
G_add_tone (SCM channel, SCM start_time, SCM tone)
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (5))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (6))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (7))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (8))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (9))),
                          // Duty
                          scm_to_double (scm_list_ref (tone, scm_from_int (10))),
                          // noise, waveform
                          false,
                          scm_to_int (scm_list_ref (tone, scm_from_int (11))));
    return SCM_UNSPECIFIED;
}


SCM
G_add_simple_tone (SCM channel, SCM start_time, SCM tone)
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          0.01,
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          0.01,
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          // Duty
                          0.5,
                          // noise, waveform
                          false, 0);
    return SCM_UNSPECIFIED;
}

SCM
G_am_add_noise (SCM channel, SCM start_time, SCM tone)
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (5))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (6))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (7))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (8))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (9))),
                          // Duty
                          scm_to_double (scm_list_ref (tone, scm_from_int (10))),
                          // noise, waveform
                          true,
                          scm_to_int (scm_list_ref (tone, scm_from_int (11))));
    return SCM_UNSPECIFIED;
}

SCM
G_am_add_simple_noise (SCM channel, SCM start_time, SCM tone)
{
    audio_model_add_tone (scm_to_int (channel),
                          scm_to_double (start_time),
                          // D_attack, D_decay, D_sustain, D_release
                          0.01,
                          scm_to_double (scm_list_ref (tone, scm_from_int (0))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (1))),
                          0.01,
                          //F_initial, F_attack, F_sustain, F_release
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (2))),
                          // A_attack, A_sustain
                          scm_to_double (scm_list_ref (tone, scm_from_int (3))),
                          scm_to_double (scm_list_ref (tone, scm_from_int (4))),
                          // Duty
                          0.5,
                          // noise, waveform
                          true, 0);
    return SCM_UNSPECIFIED;
}

SCM
G_beep ()
{
    audio_model_add_tone (0,
                          0.0,
                          // D_attack, D_decay, D_sustain, D_release
                          0.00, 0.1, 0.1, 0.1,
                          //F_initial, F_attack, F_sustain, F_release
                          440.0, 430.0, 420.0, 410.0,
                          // A_attack, A_sustain
                          1.0, 0.8,
                          // Duty
                          0.5,
                          // noise, waveform
                          false, 0);
    return SCM_UNSPECIFIED;
}

SCM
G_audio_update_time ()
{
    return scm_from_double(am_update_time);
}

SCM
G_audio_sample_index (SCM now)
{
    return scm_from_int((int)((scm_to_double(now) - am_update_time) * AUDIO_SAMPLE_RATE_IN_HZ));
}


SCM pulse_stream_read_proc;
SCM pulse_sample_rate;

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
  // g_critical ("pa_stream_connect_playback failed: %s",       pa_strerror (pa_context_errno(c)));
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
  /*        pa_strerror (pa_context_errno(c))); */
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
    /*      pa_sample_spec_snprint(tss, sizeof(tss), &sample_specification), */
    /*      pa_channel_map_snprint(tcm, sizeof(tcm), &channel_map)); */
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

SCM
pa_state (void)
{
    switch(pulse.state)
    {
    case PA_CONTEXT_UNCONNECTED:
        return scm_from_latin1_symbol("unconnected");
        break;
    case PA_CONTEXT_CONNECTING:
        return scm_from_latin1_symbol("connecting");
        break;
    case PA_CONTEXT_AUTHORIZING:
        return scm_from_latin1_symbol("authorizing");
        break;
    case PA_CONTEXT_SETTING_NAME:
        return scm_from_latin1_symbol("setting-name");
        break;
    case PA_CONTEXT_FAILED:
        return scm_from_latin1_symbol("failed");
        break;
    case PA_CONTEXT_TERMINATED:
        return scm_from_latin1_symbol("terminated");
        break;
    case PA_CONTEXT_READY:
        return scm_from_latin1_symbol("ready");
        break;
    default:
    {
        char state[10];
        snprintf(state, 9, "unknown%d", pulse.state);
        state[9] = '\0';
        return scm_from_latin1_symbol(state);
    }
        break;
    }
}

SCM
pa_is_finalized()
{
    return scm_from_bool (pulse.finalize);
}

SCM
pa_samples_written()
{
    return scm_from_uint (pulse.samples_written);
}

/* This is called when new data may be written to the stream.  */
static void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata)
{
  const pa_timing_info *timing = pa_stream_get_timing_info (p);
  unsigned n = nbytes / sizeof (uint16_t);
  unsigned microseconds_to_live = 0;

  if (timing != NULL)
      microseconds_to_live = timing->transport_usec + timing->sink_usec;
  pa_stream_update_timing_info (p, NULL, NULL);
  if (n > AUDIO_BUFFER_SIZE)
    {
        /* Buffer read overflow */
        n = AUDIO_BUFFER_SIZE;
        nbytes = n * sizeof (uint16_t);
    }
  //  SCM func = scm_variable_ref (pulse_stream_read_proc);
  if (scm_is_true (scm_procedure_p (pulse_stream_read_proc)))
      scm_call_2 (pulse_stream_read_proc, scm_from_uint (n), scm_from_uint(microseconds_to_live));

  update_sum (n);
  xpa_stream_write(p, am_sum, nbytes);

  audio_model_dequeue (n);
  pulse.samples_written += n;

  /* So, we expect the N samples to start playing in MICROSECONDS_TO_LIVE microseconds.
   * Thus, we expect the data in the audio buffers to start playing
   * in MICROSECONDS_TO_LIVE + N / SAMPLE_RATE */

    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    double now = (double) tp.tv_sec + (double) tp.tv_nsec * 1e-9;
    am_update_time = now
        + (double) microseconds_to_live / 1000000.0
        + (double) n / (double) AUDIO_SAMPLE_RATE_IN_HZ;
}


SCM
pa_finalize_audio (void)
{
    pulse_finalize_audio();
    return SCM_UNSPECIFIED;
}

void
pip_pulseaudio_init ()
{
    int i;

    SCM store = scm_c_make_vector (AUDIO_CHANNEL_COUNT, SCM_BOOL_F);
    am_channel_bytevector_store =
        scm_permanent_object (scm_c_define ("%audio-buffers", store));
    for (i = 0; i < AUDIO_CHANNEL_COUNT; i ++)
    {
        am_channel_bytevectors[i] = scm_c_make_bytevector (AUDIO_BUFFER_SIZE * sizeof (int16_t));
        scm_bytevector_fill_x (am_channel_bytevectors[i], scm_from_uint(0));
        am_channels[i] = SCM_BYTEVECTOR_CONTENTS(am_channel_bytevectors[i]);
        scm_c_vector_set_x (store, i, am_channel_bytevectors[i]);
    }

    /* We can't really get a good value of when the next call to send
     * data to pulseaudio will occur before the main loop starts
     * running. This is a rough estimate. */
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    double now = (double) tp.tv_sec + (double) tp.tv_nsec * 1e-9;
    am_update_time = now
        + (double) AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS / 1000.0;

  pulse_initialize_audio_step_1();

  pulse_stream_read_proc =
    scm_permanent_object (scm_c_define ("%audio-write-cb", SCM_BOOL_F));
  pulse_sample_rate =
      scm_permanent_object (scm_c_define ("%audio-sample-rate", scm_from_uint (AUDIO_SAMPLE_RATE_IN_HZ)));

  pulse_stream_read_proc = SCM_BOOL_F;
  scm_c_define_gsubr ("%audio-iterate", 0, 0, 0, pa_iterate);
  scm_c_define_gsubr ("%audio-state", 0, 0, 0, pa_state);
  scm_c_define_gsubr ("%audio-finalize", 0, 0, 0, pa_finalize_audio);
  scm_c_define_gsubr ("%audio-is-finalized?", 0, 0, 0, pa_is_finalized);
  scm_c_define_gsubr ("%audio-samples-written", 0, 0, 0, pa_samples_written);

  scm_c_define_gsubr ("%tone", 3, 0, 0, G_add_tone);
  scm_c_define_gsubr ("%simple-tone", 3, 0, 0, G_add_simple_tone);
  scm_c_define_gsubr ("%noise", 3, 0, 0, G_am_add_noise);
  scm_c_define_gsubr ("%simple-noise", 3, 0, 0, G_am_add_simple_noise);
  scm_c_define_gsubr ("%beep", 0, 0, 0, G_beep);
  scm_c_define_gsubr ("audio-update-time", 0, 0, 0, G_audio_update_time);
  scm_c_define_gsubr ("audio-time->index", 1, 0, 0, G_audio_sample_index);
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
