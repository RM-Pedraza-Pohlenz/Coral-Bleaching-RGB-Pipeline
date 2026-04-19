import numpy as np
import torch
import matplotlib.pyplot as plt
import cv2
from glob import glob 
from sklearn.cluster import KMeans
from collections import Counter
from skimage.color import rgb2lab, deltaE_cie76
import pandas as pd
import easyocr , os , ssl
import matplotlib.pyplot as plt
import numpy as np
ssl._create_default_https_context = ssl._create_unverified_context

# load the segment_anything library that is one folder up 
import sys
sys.path.append('../')
from segment_anything import sam_model_registry, SamAutomaticMaskGenerator

def get_image(image_path):
    image = cv2.imread(image_path)

    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    # resolution of the image for quick test 
    # image = cv2.resize(image, (1200, 800)) # 1/4 of the original image distant threshold for this ~125

    # image = preprocess_histograms( image=image)
    
    return image


def show_box(box, ax, color='green'):
    x0, y0 = box[0], box[1]
    w, h = box[2] - box[0], box[3] - box[1]
    ax.add_patch(plt.Rectangle((x0, y0), w, h, edgecolor=color, facecolor=(0,0,0,0), lw=2)) 

def is_cuda_available():
    """Checks if CUDA is available and can be used by PyTorch.

    Returns:
        bool: True if CUDA is available, False otherwise.
    """

    return torch.cuda.is_available()

def load_sam_model(model_type):
    ## Load the model for segmentation ( the SAM deep learning algorithm )
    # from segment_anything import sam_model_registry, SamAutomaticMaskGenerator
    # path_to_models = "/".join(os.getcwd().split("/")[:-1])

    sam_checkpoint = {'vit_b': '../checkpoints/vit_b_coralscop.pth'}

    if is_cuda_available():
        # The following line is for using my second GPU, free
        # device = torch.device("cuda:1")
        print("CUDA is available!")
        device = torch.device("cuda:0")
    else:
        print("CUDA is not available. Using CPU.")
        device = torch.device("cpu")

    sam = sam_model_registry[model_type](checkpoint=sam_checkpoint[model_type])
    sam.to(device=device)
    points_per_side = 32
    pred_iou_thresh = 0.72
    stability_score_thresh = 0.62
    mask_generator = SamAutomaticMaskGenerator(
        model=sam,
        points_per_side=points_per_side,
        pred_iou_thresh=pred_iou_thresh,
        stability_score_thresh=stability_score_thresh,
        crop_n_layers=1,
        crop_n_points_downscale_factor=2,
        min_mask_region_area=100,  # Requires open-cv to run post-processing
    )
    return mask_generator

class OcrAnalysis:
    """Performs analysis on OCR (Optical Character Recognition) results.

    Attributes:
        None
    """

    def __init__(self):
        """Initializes the OcrAnalysis class."""
        pass

    @staticmethod
    def get_bounding_boxes(results):
        """Extracts bounding boxes and text from OCR results.

        Args:
            results: An iterable of tuples containing individual OCR results,
                each tuple having the format (bbox, text, prob) where:
                    - bbox: A list/tuple of coordinates representing the bounding box.
                    - text: The recognized text within the bounding box.
                    - prob: The confidence probability score (optional).

        Returns:
            A tuple of two lists:
                - The first list contains bounding boxes as NumPy arrays.
                - The second list contains the corresponding recognized text.
        """

        bboxes, text_list = [], []
        for bbox, text, _ in results:
            # Extract and convert coordinates to integers
            top_left, top_right, bottom_right, bottom_left = bbox
            box = np.array([int(coord) for coord in [top_left[0], top_left[1], bottom_right[0], bottom_right[1]]])
            bboxes.append(box)
            text_list.append(text)
        return bboxes, text_list

    @staticmethod
    def get_pixels_above_bbox(bbox, image):
        """Extracts the region above the given bounding box from an image.

        Args:
            bbox: A list/tuple representing the bounding box as [x, y, width, height].
            image: The NumPy array representing the image.

        Returns:
            A NumPy array containing the cropped image region.
        """

        x, y, w, h = bbox
        box_height = 50
        # Clamp coordinates to image boundaries
        top_left_y = max(0, y - box_height)
        top_left_x = x
        bottom_right_y = y
        bottom_right_x = min(w, image.shape[1])  # Clamp right edge to image width

        cropped_image = image[top_left_y:bottom_right_y, top_left_x:bottom_right_x]
        return cropped_image
    
    @staticmethod
    def plot_custom_colorchart(custom_rgb_chart):
        # Calculate the number of squares based on the dictionary length
        num_squares = len(custom_rgb_chart)

        # Define figure size and square width
        fig, ax = plt.subplots(figsize=(10, num_squares * 0.15))
        square_width = 0.8

        # Iterate over the dictionary and plot squares
        for i, (color_name, color_value) in enumerate(custom_rgb_chart.items()):


            # Normalize color values for plotting
            normalized_color = [c / 255 for c in color_value]
            # print ( normalized_color[0], len( normalized_color[0]))


            # Calculate x position based on square width and offset
            x_pos = i * square_width

            # Create and plot the square
            square = plt.Rectangle(
                xy=(x_pos, 0), width=square_width, height=1, color=normalized_color
            )
            ax.add_patch(square)

            # Add color name label above the square
            ax.text(
                x_pos + square_width / 2,
                1.15,
                color_name,
                ha="center",
                va="center",
                fontsize=10,
                weight="bold",rotation=90
            )

        # Set axis limits and labels
        ax.set_xlim([0, num_squares * square_width])
        ax.set_ylim([-0.2, 1.3])
        ax.set_xlabel("Color Name")
        ax.set_ylabel("Color Chart")

        # Remove unnecessary ticks and grid
        ax.set_xticks([])
        ax.set_yticks([])
        ax.grid(False)

        # Show the plot
        plt.tight_layout()
        plt.show()

## Visualize images
def show_images_grid(images, titles=None, figsize=(20, 20)):
    """Displays a grid of images with optional titles."""

    num_images = len(images)
    rows = int(num_images / 2)
    cols = 2

    # Create a figure and subplots
    fig, axes = plt.subplots(rows, cols, figsize=figsize)

    # Flatten the subplots array for easier iteration
    axes = axes.flatten()

    for i, ax in enumerate(axes):
        if i < num_images:
            img = images[i]
            ax.imshow(img)
            # ax.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))  # Convert to RGB for Matplotlib
            ax.axis('off')  # Hide axes

            if titles:
                ax.set_title(titles[i])
        else:
            ax.axis('off')  # Hide unused subplots

    plt.tight_layout()
    plt.show()

# def background_to_black ( image, index ):
#     # Apply the mask to the image
#     masked_img = image.copy()
#     masked_pixels = masked_img[masks[index]['segmentation']==True]
#     masked_img[masks[index]['segmentation']==False] = (0, 0, 0)  # Set masked pixels to black
#     return masked_img ,masked_pixels

def background_to_black ( image, index , masks  ):
    # Apply the mask to the image
    masked_img = image.copy()
    masked_pixels = masked_img[masks[index]['segmentation']==True]
    masked_img[masks[index]['segmentation']==False] = (0, 0, 0)  # Set masked pixels to black
    return masked_img ,masked_pixels


def get_sorted_by_coordinates(image, anns):
    area_list=[]
    cropped_image_dic ={}
    mask_number = [] 
    mask_pixles_dic = {}
    X_coord_list , Y_coord_list = [] , [] 
    for i in range(len(anns)):
        x, y, width, height = anns[i]['bbox']
        area = anns[i]["area"]
        # image_b, masked_pixels = background_to_black(image=image, index=i)
        image_b, masked_pixels = background_to_black(image=image, index=i , masks=anns)
        cropped_image = image_b[int(y):int(y+height), int(x):int(x+width)]
        x_coord, y_coord = anns[i]['point_coords'][0]
        X_coord_list.append(x_coord) 
        Y_coord_list.append(y_coord)


        area_list.append(area)
        cropped_image_dic[i] = cropped_image
        mask_pixles_dic[i] = masked_pixels
        mask_number.append(i)
    df = pd.DataFrame([area_list,mask_number,X_coord_list,Y_coord_list])
    df = df.T
    df.columns = ['area','mask_number','X_corrd','Y_coord']
    df.sort_values(by=['Y_coord','X_corrd'], ascending=True, inplace=True)
    df.reset_index(drop=True, inplace=True)
    df.dropna(inplace=True)
    # cropped_image_dic has the same order of the masks number
    return df , cropped_image_dic , mask_pixles_dic

def process_images_and_sort_by_coordinates(image, masks):
    image_dataframe, cropped_image_list , mask_pixels_dict = get_sorted_by_coordinates( image=image , anns=masks )
    mask_number_list = image_dataframe['mask_number'].to_list()
    list_of_images = [ cropped_image_list [idx ] for idx in mask_number_list  ]
    print (image_dataframe.columns)
    # print the first 5 rows of the dataframe
    print (image_dataframe.head(n=10))

    titles = [  ] 
    for idx in image_dataframe.index.to_list() :
        titles.append(f"index_{idx}")
    return list_of_images , titles , image_dataframe

def experiment_grid(model_type, image):
    mask_generator = load_sam_model(model_type=model_type)
    masks = mask_generator.generate(image=image)
    list_of_images , titles , image_dataframe = process_images_and_sort_by_coordinates(image = image, masks= masks)
    show_images_grid( images=list_of_images , titles=titles )

