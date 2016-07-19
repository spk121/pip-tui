(define-module (pip-tui pulseaudio)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (pip-tui action)
  #:export (%audio-buffers
            audio-iterate
            audio-state
            audio-finalize
            audio-is-finalized?
            audio-samples-written
            audio-update-time
            audio-time->index
            tone
            simple-tone
            noise
            simple-noise
            Beep
	    pulseaudio-set-write-cb!
	    pulseaudio-idle-handler))

(define (audio-iterate)
  "This must be called at regular intervals in the main
loop so that the audio is processed."
  (%audio-iterate))

(define (audio-state)
  "This returns a symbol that described the current state
of the pulseaudio interface.  One of 'unconnected, 'connecting,
'authorizing, 'setting-name, 'failed, 'terminated', or 'ready"
  (%audio-state))

(define (audio-finalize)
  "This will shut down the audio system gracefully."
  (%audio-finalize))

(define (audio-is-finalized?)
  "Returns #t if the audio system has been shut down with (audio-finalize)"
  (%audio-is-finalized?))

(define (audio-samples-written)
  "Returns a count of the number of samples written to the audio
system since it initialized."
  (%audio-samples-written))

(define (tone channel time parameters)
  "Then enqueues a tone to play on the given CHANNEL, starting at the given TIME,
as described by a list of PARAMETERS.  PARAMETERS is a list of 12 numeric values
which are
DURATION_ATTACK, DURATION_DECAY, DURATION_SUSTAIN, DURATION_RELEASE in seconds
FREQUENCY_INITIAL, FREQUENCY_ATTACK, FREQUENCY_SUSTAIN, FREQUENCY_RELEASE in Hz
AMPLITUDE_ATTACK, AMPLITUDE_SUSTAIN from 0.0 to 1.0
DUTY, which is ratio of up to down in the waveform from ~0.3 to ~0.7,
WAVEFORM 0=square 1=sine"
  (%tone channel time parameters))

(define (simple-tone channel time parameters)
  "Then enqueues a square wave tone play on the given CHANNEL, starting at the given TIME,
as described by a list of PARAMETERS.  PARAMETERS is a list of 5 numeric values
which are
DURATION_DECAY, DURATION_SUSTAIN in seconds
FREQUENCY in Hz
AMPLITUDE_ATTACK, AMPLITUDE_SUSTAIN from 0.0 to 1.0"
  (%simple-tone channel time parameters))

(define (noise channel time parameters)
  "Then enqueues a noise to play on the given CHANNEL, starting at the given TIME,
as described by a list of PARAMETERS.  PARAMETERS is a list of 12 numeric values
which are
DURATION_ATTACK, DURATION_DECAY, DURATION_SUSTAIN, DURATION_RELEASE in seconds
FREQUENCY_INITIAL, FREQUENCY_ATTACK, FREQUENCY_SUSTAIN, FREQUENCY_RELEASE in Hz
AMPLITUDE_ATTACK, AMPLITUDE_SUSTAIN from 0 to 1.0
DUTY, which is ratio of up to down in the waveform from ~0.3 to ~0.7,
WAVEFORM 0=square 1=sine"
  (%noise channel time parameters))

(define (simple-noise channel time parameters)
  "Then enqueues a square wave noise to play on the given CHANNEL, starting at the given TIME,
as described by a list of PARAMETERS.  PARAMETERS is a list of 5 numeric values
which are
DURATION_DECAY, DURATION_SUSTAIN in seconds
FREQUENCY in Hz
AMPLITUDE_ATTACK, AMPLITUDE_SUSTAIN from 0 to 1.0"
  (%simple-noise channel time parameters))

(define (Beep)
  "This enqueues a beep to play on channel zero as soon as possible."
  (%beep))


(define (pulseaudio-set-write-cb! func)
  "This registers a callback function to be called whenever Pulseaudio
is preparing to write new samples.  The callback function should take
two parameters: N and TIME-TO-LIVE.  N is the number of samples being
requested, and TIME-TO-LIVE is the number of microseconds until these
samples actually play."
  (set! %audio-write-cb func))

(define (audio-idle-action-activate TT event state)
  (when (idle-event? event)
        (audio-iterate)))

(define (pulseaudio-idle-handler)
  (action-new "audio-idle" #t '() audio-idle-action-activate #f))


(load-extension "piptui" "pip_pulseaudio_init")

