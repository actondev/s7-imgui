#include "stb_image.h"
#include "math.h"
namespace aod {

namespace img {

/**
 * Refering stbi_image_free:
 * Could use something like..
 *
 *
 auto deleter=[&](int* ptr){...};
std::unique_ptr<int[], decltype(deleter)> ptr4(new int[4], deleter);

 */

double normalized_correlation_coefficient(unsigned char * X, unsigned  char* Y, size_t n) {
    // implementation from https://www.geeksforgeeks.org/program-find-correlation-coefficient/
    // https://en.wikipedia.org/wiki/Correlation_and_dependence Sample correlation coefficient
    // adjusted for math rounding errors
    double sum_X = 0, sum_Y = 0, sum_XY = 0;
    double squareSum_X = 0, squareSum_Y = 0;

    for (size_t i = 0; i < n; i++) {
        double xi = (double)X[i] / 255.0;
        double yi = (double)Y[i] / 255.0;

        // sum of elements of array X.
        sum_X = sum_X + xi;

        // sum of elements of array Y.
        sum_Y = sum_Y + yi;

        // sum of X[i] * Y[i].
        sum_XY = sum_XY + xi * yi;

        // sum of square of array elements.
        squareSum_X = squareSum_X + xi * xi;
        squareSum_Y = squareSum_Y + yi * yi;
    }

    // use formula for calculating correlation coefficient.
    double numer = (n * sum_XY - sum_X * sum_Y);
    double denumer_sq = (n * squareSum_X - sum_X * sum_X) * (n * squareSum_Y - sum_Y * sum_Y);
    double corr = numer / sqrt(denumer_sq);

    return corr;
}



bool are_equivalent(const char* img1, const char* img2) {
    int w1, h1, bit_depth1;
    int w2, h2, bit_depth2;
    unsigned char *data1 = stbi_load(img1, &w1, &h1, &bit_depth1, 0);
    unsigned char *data2 = stbi_load(img2, &w2, &h2, &bit_depth2, 0);
    bool res = false;

    do {
        // inner comparison
        // quickly breaking out
        // this is cause we have to release the resources later on
        // could I use some std::unique_ptr thing? (with deleter)
        if (data1 == NULL || data2 == NULL) {
            break;
        }

        if (w1 == 0 || h1 == 0 || w2 == 0 || h2 == 0) {
            break;
        }

        if (w1 != w2 || h1 != h2) {
            break;
        }

        if (bit_depth1 != bit_depth2) {
            break;
        }

        double corr = normalized_correlation_coefficient(data1, data2, w1 * h1);
//         printf("corr is %f\n", corr);

        if (corr > 0.9) {
            res = true;
        }

    } while (false);
    stbi_image_free(data1);
    stbi_image_free(data2);
    return res;
}

} // img
}// aod
