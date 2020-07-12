#include "SDL.h"
#include "SDL_audio.h"
#include <memory>

namespace aod {
namespace sdl {

// experimental class, not really useful
class AudioObject {
private:
    bool freed_wav = false;
    inline AudioObject() {};
    Uint32 wav_length; // length of our sample
    Uint8 *wav_buffer = nullptr; // buffer containing our audio file
    SDL_AudioSpec wav_spec; // the specs of our piece of music
    void onCallback(Uint8 *stream, int len);
    static void forwardCallback(void *userdata, Uint8 *stream, int len);
    Uint8 *audio_pos;
    Uint32 audio_len;
    int id = 0;

public:
    ~AudioObject();
    static std::unique_ptr<AudioObject> fromFile(const char*);
    bool openAudio();
    void play();
    void stop();
    bool finished();
    void freeWav();
    void rewind();
    // sets the *wav_buffer to a new pointer. obviously, when played back it will be garbage
    // (hope it doesn't crash though)
    void glitch();
    inline void setId(int id) {
        this->id = id;
    }
};
}
}
