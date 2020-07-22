#include "midi.hpp"

namespace aod {
namespace midi {

// from iPlugMidi.h
struct IMidiMsg {
    uint8_t mStatus, mData1, mData2;
    enum EStatusMsg {
        kNone = 0,
        kNoteOff = 8,
        kNoteOn = 9,
        kPolyAftertouch = 10,
        kControlChange = 11,
        kProgramChange = 12,
        kChannelAftertouch = 13,
        kPitchWheel = 14
    };

    EStatusMsg StatusMsg() const {
        unsigned int e = mStatus >> 4;
        if (e < kNoteOff || e > kPitchWheel) {
            return kNone;
        }
        return (EStatusMsg) e;
    }
    /** @return [0, 127), -1 if NA. */
    int NoteNumber() const {
        switch (StatusMsg()) {
        case kNoteOn:
        case kNoteOff:
        case kPolyAftertouch:
            return mData1;
        default:
            return -1;
        }
    }

    /** @return returns [0, 127), -1 if NA. */
    int Velocity() const {
        switch (StatusMsg()) {
        case kNoteOn:
        case kNoteOff:
            return mData2;
        default:
            return -1;
        }
    }
    
    // some helpers

    bool is_note_on() const {
        return StatusMsg() == kNoteOn;
    }

    bool is_note_off() const {
        return StatusMsg() == kNoteOff;
    }

};

bool is_note_on(uint8_t status, uint8_t data1, uint8_t data2) {
    IMidiMsg msg = {status, data1, data2};
    return msg.is_note_on() && msg.Velocity() > 0;
}

bool is_note_off(uint8_t status, uint8_t data1, uint8_t data2) {
    IMidiMsg msg = {status, data1, data2};
    // hm.. I noticed that with my KeystationMini32 and RtMidi, I don't get kNoteOff
    // but instead kNoteOn and velocity 0

    return msg.is_note_off() || msg.Velocity() == 0;
}

int note_number(uint8_t status, uint8_t data1, uint8_t data2) {
    IMidiMsg msg = {status, data1, data2};
    return msg.NoteNumber();
}

int note_velocity(uint8_t status, uint8_t data1, uint8_t data2) {
    IMidiMsg msg = {status, data1, data2};
    return msg.Velocity();
}

}
}


