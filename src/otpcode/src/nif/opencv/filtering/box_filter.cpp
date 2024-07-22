#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to apply Box Filter
static ERL_NIF_TERM box_filter_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 8) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int ddepth, ksize_width, ksize_height, anchor_x, anchor_y;
    int normalize;
    int borderType;
    if (!enif_get_int(env, argv[1], &ddepth) ||
        !enif_get_int(env, argv[2], &ksize_width) ||
        !enif_get_int(env, argv[3], &ksize_height) ||
        !enif_get_int(env, argv[4], &anchor_x) ||
        !enif_get_int(env, argv[5], &anchor_y) ||
        !enif_get_int(env, argv[6], &normalize) ||
        !enif_get_int(env, argv[7], &borderType)) {
        return enif_make_badarg(env);
    }

    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    Mat src = imread(imagePath, IMREAD_COLOR);
    if (src.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    Mat dst;
    Size ksize(ksize_width, ksize_height);
    Point anchor(anchor_x, anchor_y);

    // Validate and adjust the anchor point
    if (anchor_x == -1 && anchor_y == -1) {
        anchor = Point(ksize_width / 2, ksize_height / 2);
    } else if (anchor_x < 0 || anchor_x >= ksize_width || anchor_y < 0 || anchor_y >= ksize_height) {
        return enif_make_string(env, "Invalid anchor point", ERL_NIF_LATIN1);
    }

    boxFilter(src, dst, ddepth, ksize, anchor, normalize, borderType);

    string outputPath = "box_filtered_image.jpg";
    imwrite(outputPath, dst);

    return enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 8, box_filter_nif}
};

ERL_NIF_INIT(box_filter, nif_funcs, NULL, NULL, NULL, NULL)
