#include "s7.h"
#include "stdio.h"
#include "aod/nfd.hpp"

int main(int argc, char **argv) {
    bool blah = false;
    if(blah){
        // apparently, the nfd bindings in s7 were causing many error reports
        // so, generating a suppersion file from this program
        auto file = aod::nfd::open();
        (void)file;
        auto file2 = aod::nfd::save();
    }
}
