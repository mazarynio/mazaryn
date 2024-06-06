#include <opencv2/opencv.hpp>
#include <iostream>

using namespace cv;
using namespace std;

int main(int argc, char** argv) {
    if (argc != 2) {
        cout << "Usage: " << argv[0] << " <image_path>" << endl;
        return -1;
    }

    string imagePath = argv[1];
    Mat image = imread(imagePath, IMREAD_COLOR);

    if (image.empty()) {
        cerr << "Could not open or find the image" << endl;
        return -1;
    }

    // Define the rectangle that includes the object to segment
    Rect rectangle(50, 50, image.cols - 100, image.rows - 100);

    Mat result;
    Mat bgModel, fgModel; // Temporary arrays for the models
    Mat mask(image.size(), CV_8UC1, Scalar(GC_BGD)); // Initial mask

    // Initialize mask with a rectangle
    grabCut(image, mask, rectangle, bgModel, fgModel, 5, GC_INIT_WITH_RECT);

    // Modify the mask such that all probable foreground pixels are marked as definite foreground
    compare(mask, GC_PR_FGD, mask, CMP_EQ);
    Mat foreground(image.size(), CV_8UC3, Scalar(255, 255, 255));
    image.copyTo(foreground, mask);

    // Display results
    namedWindow("Original Image", WINDOW_AUTOSIZE);
    imshow("Original Image", image);

    namedWindow("Segmented Image", WINDOW_AUTOSIZE);
    imshow("Segmented Image", foreground);

    waitKey(0);
    return 0;
}
