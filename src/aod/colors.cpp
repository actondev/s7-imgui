#include <array>
namespace aod {
namespace colors {

// see https://github.com/bahamas10/ryb/blob/d81102f/js/RXB.js#L252-L330
namespace ryb {

const double magic[8][3] = {
    {1,     1,     1},
    {1,     1,     0},
    {1,     0,     0},
    {1,     0.5,   0},
    {0.163, 0.373, 0.6},
    {0.0,   0.66,  0.2},
    {0.5,   0.0,   0.5},
    {0.2,   0.094, 0.0}
};

double cubicInt(double t, double A, double B) {
    double weight = t * t * (3 - 2 * t);
    return A + weight * (B - A);
}

double getR(double iR, double iY, double iB) {
    // red
    double x0 = cubicInt(iB, magic[0][0], magic[4][0]);
    double x1 = cubicInt(iB, magic[1][0], magic[5][0]);
    double x2 = cubicInt(iB, magic[2][0], magic[6][0]);
    double x3 = cubicInt(iB, magic[3][0], magic[7][0]);
    double y0 = cubicInt(iY, x0, x1);
    double y1 = cubicInt(iY, x2, x3);
    return cubicInt(iR, y0, y1);
}

double getG(double iR, double iY, double iB) {
    double x0 = cubicInt(iB, magic[0][1], magic[4][1]);
    double x1 = cubicInt(iB, magic[1][1], magic[5][1]);
    double x2 = cubicInt(iB, magic[2][1], magic[6][1]);
    double x3 = cubicInt(iB, magic[3][1], magic[7][1]);
    double y0 = cubicInt(iY, x0, x1);
    double y1 = cubicInt(iY, x2, x3);
    return cubicInt(iR, y0, y1);
}

double getB(double iR, double iY, double iB) {
    // blue
    double x0 = cubicInt(iB, magic[0][2], magic[4][2]);
    double x1 = cubicInt(iB, magic[1][2], magic[5][2]);
    double x2 = cubicInt(iB, magic[2][2], magic[6][2]);
    double x3 = cubicInt(iB, magic[3][2], magic[7][2]);
    double y0 = cubicInt(iY, x0, x1);
    double y1 = cubicInt(iY, x2, x3);
    return cubicInt(iR, y0, y1);
}

/**
 *   var ryb2rgb = (function() {
    // see http://threekings.tk/mirror/ryb_TR.pdf
    function cubicInt(t, A, B){
      var weight = t * t * (3 - 2 * t);
      return A + weight * (B - A);
    }

    function getR(iR, iY, iB, magic) {
      magic = magic || MAGIC_COLORS;
      // red
      var x0 = cubicInt(iB, magic[0][0], magic[4][0]);
      var x1 = cubicInt(iB, magic[1][0], magic[5][0]);
      var x2 = cubicInt(iB, magic[2][0], magic[6][0]);
      var x3 = cubicInt(iB, magic[3][0], magic[7][0]);
      var y0 = cubicInt(iY, x0, x1);
      var y1 = cubicInt(iY, x2, x3);
      return cubicInt(iR, y0, y1);
    }

    function getG(iR, iY, iB, magic) {
      magic = magic || MAGIC_COLORS;
      // green
      var x0 = cubicInt(iB, magic[0][1], magic[4][1]);
      var x1 = cubicInt(iB, magic[1][1], magic[5][1]);
      var x2 = cubicInt(iB, magic[2][1], magic[6][1]);
      var x3 = cubicInt(iB, magic[3][1], magic[7][1]);
      var y0 = cubicInt(iY, x0, x1);
      var y1 = cubicInt(iY, x2, x3);
      return cubicInt(iR, y0, y1);
    }

    function getB(iR, iY, iB, magic) {
      magic = magic || MAGIC_COLORS;
      // blue
      var x0 = cubicInt(iB, magic[0][2], magic[4][2]);
      var x1 = cubicInt(iB, magic[1][2], magic[5][2]);
      var x2 = cubicInt(iB, magic[2][2], magic[6][2]);
      var x3 = cubicInt(iB, magic[3][2], magic[7][2]);
      var y0 = cubicInt(iY, x0, x1);
      var y1 = cubicInt(iY, x2, x3);
      return cubicInt(iR, y0, y1);
    }

    function ryb2rgb(color, limit, magic) {
      limit = limit || 255;
      magic = magic || MAGIC_COLORS;
      var R = color[0] / limit;
      var Y = color[1] / limit;
      var B = color[2] / limit;
      var R1 = getR(R, Y, B, magic);
      var G1 = getG(R, Y, B, magic);
      var B1 = getB(R, Y, B, magic);
      return [
        Math.ceil(R1 * limit),
        Math.ceil(G1 * limit),
        Math.ceil(B1 * limit)
      ];
    }
    return ryb2rgb;
  })();

  */
}

std::array<double, 3> ryb2rgb(std::array<double, 3> ryb) {

    double R = ryb[0];
    double Y = ryb[1];
    double B = ryb[2];
    double R1 = ryb::getR(R, Y, B);
    double G1 = ryb::getG(R, Y, B);
    double B1 = ryb::getB(R, Y, B);
    return {R1, G1, B1};
}

} // colors
} // aod
