#pragma once
#include <cstdint>

namespace aod {
namespace midi {

bool is_note_on(uint8_t status, uint8_t data1, uint8_t data2);

bool is_note_off(uint8_t status, uint8_t data1, uint8_t data2);

int note_number(uint8_t status, uint8_t data1, uint8_t data2);

int note_velocity(uint8_t status, uint8_t data1, uint8_t data2);

}
}


