#include "./audio.hpp"

namespace aod {
namespace sdl {

// https://gist.github.com/armornick/3447121
// https://stackoverflow.com/questions/16838920/pointer-to-member-function-for-sdl-audio-callback

// https://stackoverflow.com/questions/23844964/c-static-factory-constructor
// could just return a raw pointer and let the caller put it in a unique_ptr,
// but wanted to test this
std::unique_ptr<AudioObject> AudioObject::fromFile(const char* path) {

    std::unique_ptr<AudioObject> obj(new AudioObject());

    if (SDL_LoadWAV(path, &obj->wav_spec, &obj->wav_buffer, &obj->wav_length) == NULL) {
        fprintf(stderr, "Error: %s\n", SDL_GetError());
        return std::unique_ptr<AudioObject> {};
    }

    obj->wav_spec.userdata = obj.get();
    obj->wav_spec.callback = AudioObject::forwardCallback;

    obj->audio_pos = obj->wav_buffer; // copy sound buffer
    obj->audio_len = obj->wav_length; // copy file length
    return obj;
//     return std::move(obj);
}

AudioObject::~AudioObject() {
    fprintf(stderr, "AudioObject dtor\n");
    freeWav();
}


bool AudioObject::openAudio() {
    return SDL_OpenAudio(&wav_spec, NULL) >= 0;
}

void sdl::AudioObject::forwardCallback(void* userdata, Uint8* stream, int len) {
    static_cast<AudioObject*>(userdata)->onCallback(stream, len);
}

void AudioObject::onCallback(Uint8* stream, int len) {
    if (audio_len == 0) {
        stop();
        return;
    }

    len = (len > audio_len ? audio_len : len);
    SDL_memcpy(stream, audio_pos, len); // simply copy from one buffer into the other
    // SDL_MixAudio(stream, audio_pos, len, SDL_MIX_MAXVOLUME);// mix from one buffer into another

    audio_pos += len;
    audio_len -= len;
}

bool AudioObject::finished() {
    return audio_len == 0;
}

void AudioObject::freeWav() {
    // if (wav_buffer != NULL) {
    if (!freed_wav) {
        SDL_FreeWAV(wav_buffer);
        freed_wav = true;
    } else {
        fprintf(stderr, "Have already freed\n");
    }
    // having fun with glitch, not nulling the buffer pointer
    // wav_buffer = NULL;
}

void AudioObject::rewind() {
    audio_pos = wav_buffer;
    audio_len = wav_length;
}

void AudioObject::play() {
    openAudio();
    rewind();
    SDL_PauseAudio(0);
}

void AudioObject::glitch() {
    Uint8 *temp;
    // :o ! uninitialized hehe
    wav_buffer = temp;
}


void AudioObject::stop() {
    fprintf(stderr, "stop\n");
    SDL_CloseAudio();
}

}
}
