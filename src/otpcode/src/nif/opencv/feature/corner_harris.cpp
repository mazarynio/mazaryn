#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply cornerHarris
static ERL_NIF_TERM corner_harris_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 6) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    char outputPath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[1], outputPath, sizeof(outputPath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int blockSize, ksize;
    double k;
    int borderType;
    if (!enif_get_int(env, argv[2], &blockSize) ||
        !enif_get_int(env, argv[3], &ksize) ||
        !enif_get_double(env, argv[4], &k) ||
        !enif_get_int(env, argv[5], &borderType)) {
        return enif_make_badarg(env);
    }

    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    Mat src = imread(imagePath, IMREAD_GRAYSCALE);
    if (src.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    Mat dst;
    cornerHarris(src, dst, blockSize, ksize, k, borderType);

    // Normalize for visualization
    Mat dst_norm, dst_norm_scaled;
    normalize(dst, dst_norm, 0, 255, NORM_MINMAX, CV_32FC1, Mat());
    convertScaleAbs(dst_norm, dst_norm_scaled);

    imwrite(outputPath, dst_norm_scaled);

    return enif_make_string(env, outputPath, ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 6, corner_harris_nif}
};

ERL_NIF_INIT(corner_harris, nif_funcs, NULL, NULL, NULL, NULL)
