#include <opencv2/opencv.hpp>
#include <iostream>
#include "erl_nif.h"
#include <filesystem> // For checking the existence of the file

using namespace cv;
using namespace std;
namespace fs = std::filesystem;

// Function to perform GrabCut segmentation
static ERL_NIF_TERM grabcut_segment_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Ensure we have the correct number of arguments
    if (argc != 5) {
        return enif_make_badarg(env);
    }

    char imagePath[256];
    int x, y, width, height;

    // Validate and extract arguments
    if (!enif_get_string(env, argv[0], imagePath, sizeof(imagePath), ERL_NIF_LATIN1) ||
        !enif_get_int(env, argv[1], &x) ||
        !enif_get_int(env, argv[2], &y) ||
        !enif_get_int(env, argv[3], &width) ||
        !enif_get_int(env, argv[4], &height)) {
        return enif_make_badarg(env);
    }

    // Print the absolute path for debugging
    cout << "Image path: " << imagePath << endl;
    if (!fs::exists(imagePath)) {
        cerr << "Error: File does not exist: " << imagePath << endl;
        return enif_make_string(env, "File does not exist", ERL_NIF_LATIN1);
    }

    // Read the input image
    Mat image = imread(imagePath, IMREAD_COLOR);

    if (image.empty()) {
        cerr << "Could not open or find the image" << endl;
        return enif_make_string(env, "Could not open or find the image", ERL_NIF_LATIN1);
    }

    // Define the rectangle that includes the object to segment
    Rect rectangle(x, y, width, height);

    Mat result;
    Mat bgModel, fgModel; // Temporary arrays for the models
    Mat mask(image.size(), CV_8UC1, Scalar(GC_BGD)); // Initial mask

    // Initialize mask with a rectangle
    mask(rectangle).setTo(Scalar(GC_PR_FGD));

    // Run GrabCut
    grabCut(image, mask, rectangle, bgModel, fgModel, 5, GC_INIT_WITH_RECT);

    // Modify the mask such that all probable foreground pixels are marked as definite foreground
    compare(mask, GC_PR_FGD, mask, CMP_EQ);
    Mat foreground(image.size(), CV_8UC3, Scalar(255, 255, 255));
    image.copyTo(foreground, mask);

    // Save the segmented image to disk (for demonstration purposes)
    string outputPath = "segmented_image.jpg";
    imwrite(outputPath, foreground);

    return enif_make_string(env, outputPath.c_str(), ERL_NIF_LATIN1);
}

// NIF initialization
static ErlNifFunc nif_funcs[] = {
    {"grabcut_segment", 5, grabcut_segment_nif}
};

ERL_NIF_INIT(grabcut, nif_funcs, NULL, NULL, NULL, NULL)
