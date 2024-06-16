#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem>

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Helper function to convert Erlang boolean to C++ int
static int enif_get_boolean(ErlNifEnv* env, ERL_NIF_TERM term, int* b) {
    if (enif_is_identical(term, enif_make_atom(env, "true"))) {
        *b = 1;
        return 1;
    } else if (enif_is_identical(term, enif_make_atom(env, "false"))) {
        *b = 0;
        return 1;
    }
    return 0;
}

// Function to apply calcBackProject
static ERL_NIF_TERM calc_back_project_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 7) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    char histPath[256];
    char outputPath[256];
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[1], histPath, sizeof(histPath), ERL_NIF_LATIN1) ||
        !enif_get_string(env, argv[6], outputPath, sizeof(outputPath), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int channels;
    double scale;
    int uniform;
    ERL_NIF_TERM rangesTerm;
    if (!enif_get_int(env, argv[2], &channels) ||
        !enif_get_double(env, argv[3], &scale) ||
        !enif_get_boolean(env, argv[4], &uniform) || // Ensure uniform is correctly processed
        !enif_is_list(env, argv[5])) {
        return enif_make_badarg(env);
    }
    rangesTerm = argv[5];

    cout << "Image path: " << imagePath << endl;
    cout << "Histogram path: " << histPath << endl;

    if (!fs::exists(imagePath) || !fs::exists(histPath)) {
        cerr << "Error: One or both of the files do not exist" << endl;
        return enif_make_string(env, "One or both of the files do not exist", ERL_NIF_LATIN1);
    }

    Mat src = imread(imagePath, IMREAD_COLOR);
    if (src.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    FileStorage fs(histPath, FileStorage::READ);
    if (!fs.isOpened()) {
        cerr << "Could not open or find the histogram file" << endl;
        return enif_make_string(env, "Could not open or find the histogram file", ERL_NIF_LATIN1);
    }
    Mat hist;
    fs["histogram"] >> hist;
    fs.release();

    // Extract ranges from the list
    vector<float> rangeValues;
    ERL_NIF_TERM head, tail;
    double rangeValue;
    while (enif_get_list_cell(env, rangesTerm, &head, &tail)) {
        if (!enif_get_double(env, head, &rangeValue)) {
            return enif_make_badarg(env);
        }
        rangeValues.push_back(static_cast<float>(rangeValue));
        rangesTerm = tail;
    }
    const float* ranges[] = { rangeValues.data() };

    Mat backProject;
    calcBackProject(&src, 1, &channels, hist, backProject, ranges, scale, uniform != 0);

    imwrite(outputPath, backProject);

    return enif_make_string(env, outputPath, ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"apply", 7, calc_back_project_nif}
};

ERL_NIF_INIT(calc_back_project, nif_funcs, NULL, NULL, NULL, NULL)
