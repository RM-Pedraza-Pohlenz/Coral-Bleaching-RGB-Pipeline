# %%
# load manipilation_library
import sys
sys.path.append('./manipulation_library.py') # add the path of manipulation_library.py
from manipulation_library import *
import os
import glob
import argparse
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# %%
def get_center(bbox):
    """
    Calculate the center of a bounding box.
    bbox: [x_min, y_min, x_max, y_max]
    """
    x_center = (bbox[0] + bbox[2]) / 2
    y_center = (bbox[1] + bbox[3]) / 2
    return np.array([x_center, y_center])

def euclidean_distance(point1, point2):
    """
    Calculate the Euclidean distance between two points.
    """
    return np.linalg.norm(point1 - point2)

def find_close_bounding_boxes(ocr_bboxes, seg_bboxes, threshold):
    """
    Find OCR bounding boxes that are close to segmentation bounding boxes.
    ocr_bboxes: List of OCR bounding boxes
    seg_bboxes: List of segmentation bounding boxes
    threshold: Distance threshold to consider bounding boxes as close
    """
    close_bboxes = []
    close_bboxes_indices = []
    for idx_ocr, ocr_bbox in enumerate(ocr_bboxes):
        ocr_center = get_center(ocr_bbox)
        for idx, seg_bbox in enumerate(seg_bboxes):
            seg_center = get_center(seg_bbox)
            distance = euclidean_distance(ocr_center, seg_center)
            if distance <= threshold and ocr_bbox[1] < seg_bbox[1]:
                close_bboxes.append((ocr_bbox, seg_bbox))
                close_bboxes_indices.append((idx, idx_ocr))
    return close_bboxes, close_bboxes_indices

def find_unpaired_seg_bboxes(seg_bboxes, close_bboxes_indices):
    
    """
    Find segmentation bounding boxes that do not have a paired OCR bounding box.
    seg_bboxes: List of segmentation bounding boxes
    close_bboxes_indices: List of indices of close bounding boxes
    """
    paired_seg_indices = {pair[0] for pair in close_bboxes_indices}
    unpaired_seg_indices = [idx for idx in range(len(seg_bboxes)) if idx not in paired_seg_indices]
    return unpaired_seg_indices

# %%
# def background_to_black ( image, index , masks  ):
#     # Apply the mask to the image
#     masked_img = image.copy()
#     masked_pixels = masked_img[masks[index]['segmentation']==True]
#     masked_img[masks[index]['segmentation']==False] = (0, 0, 0)  # Set masked pixels to black
#     return masked_img ,masked_pixels


def use_sorted_mask(image, masks):
    cropped_image_list = []
    for i in range(len(masks)):
        x, y, width, height = masks[i]['bbox']
        image_b, masked_pixels = background_to_black(image=image, index=i , masks=masks)
        cropped_image = image_b[int(y):int(y+height), int(x):int(x+width)]
        cropped_image_list.append(cropped_image)

    return cropped_image_list

# %%
def process_images_and_use_sorted_mask(image, masks):
    cropped_image_list  = use_sorted_mask( image=image , masks=masks )
    return cropped_image_list

# %%

# # Example usage
# ocr_bboxes = [[10, 20, 50, 60], [100, 120, 150, 160]]  # Replace with actual OCR bounding boxes
# seg_bboxes = [[12, 22, 52, 62], [200, 220, 250, 260]]  # Replace with actual segmentation bounding boxes
# threshold = 10  # Define your threshold distance

# close_bboxes = find_close_bounding_boxes(ocr_bboxes, seg_bboxes, threshold)
# print(close_bboxes)

# %%

# Function to get user input using argparse

def get_user_input():
    """ 
    Get user input using argparse. The usage is as follows:

    python segmetation_ocr_script.py ../data/01_raw/test_10 375 clockwise 
    
    The first argument is the main path to the images.
    The second argument is the threshold distance to consider bounding boxes as close. this is related to the distance between the center of the bounding boxes, and the threshold is in pixels. 
    Meaning this is second argumet is related to the size of the image. E.g for images of size 1200 x 800, a threshold of 125. 
    In this case the threshold is 375, which is related to the size of the image 3600 x 2400 (this is hardcoded on manipulation library).
    The third argument is the rotation option, which can be clockwise, counterclockwise, or none.
    """
    parser = argparse.ArgumentParser(description="Process images and find close bounding boxes.")
    parser.add_argument('main_path', type=str, help='The main path to the images.')
    parser.add_argument('threshold', type=int, help='The threshold distance to consider bounding boxes as close.')
    parser.add_argument('rotate_option', type=str, choices=['clockwise', 'counterclockwise', 'none'], help='Rotate image? (clockwise/counterclockwise/none)')
    
    try:
        args = parser.parse_args()
        return args.main_path, args.threshold, args.rotate_option
    except argparse.ArgumentError:
        parser.print_usage()
        sys.exit(1)

# Get user input
main_path, threshold, rotate_option = get_user_input()

# Find all image files in the main path
image_files = glob.glob(os.path.join(main_path, '*'))

# Load the OCR model and SAM model
mask_generator = load_sam_model(model_type="vit_b")
reader = easyocr.Reader(['en'],gpu=True) # this needs to run only once to load the model into memory


# Process each image file
for image_file in image_files:
    image = get_image(image_file)
    image_name = os.path.basename(image_file)[0:-4]
    print (f'Processing image: {image_name}')

    # Create output directory if it doesn't exist
    output_dir = f'../data/02_interim/{image_name}'
    if not os.path.exists(output_dir):
        os.makedirs(output_dir ,exist_ok=True)
    
    # Rotate the image based on user input
    if rotate_option == 'clockwise':
        image = cv2.rotate(image, cv2.ROTATE_90_CLOCKWISE)
    elif rotate_option == 'counterclockwise':
        image = cv2.rotate(image, cv2.ROTATE_90_COUNTERCLOCKWISE)
    else: # no rotation option is none
        pass
    

    # %%
    # create a copy of the image in grayscale
    image_gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    result = reader.readtext(image_gray)
    ocr_bboxes, text_list = OcrAnalysis.get_bounding_boxes(result)

    # %%
    masks = mask_generator.generate(image=image)
    # list_of_images , titles , image_dataframe = process_images_and_sort_by_coordinates(image = image, masks= masks)

    # %%
    seg_bboxes = [    ]
    for i in range(len(masks)):
        x, y, width, height = masks[i]['bbox']
        seg_bboxes.append ( np.array([x, y, x+width, y+height]) ) 

    # %%
    close_bboxes = find_close_bounding_boxes(ocr_bboxes, seg_bboxes, threshold)

    # %%
    # print(close_bboxes)

    # # %%
    # len(close_bboxes)

    # %%
    # for i in range(100,150,5):
    #     threshold = i
    #     close_bboxes ,close_bboxes_indices = find_close_bounding_boxes(ocr_bboxes, seg_bboxes, threshold)
    #     print(f'{threshold} : {len(close_bboxes)} , {close_bboxes_indices}')

    # Find close bounding boxes with the new threshold
    close_bboxes, close_bboxes_indices = find_close_bounding_boxes(ocr_bboxes, seg_bboxes, threshold)

    # Create a figure and axis
    fig, ax = plt.subplots(1, figsize=(12, 12))

    # Display the image
    ax.imshow(image)

    # Define colors for bounding boxes
    colors = plt.get_cmap('tab20', len(close_bboxes))

    # Plot OCR and segmentation bounding boxes
    for idx, (ocr_bbox, seg_bbox) in enumerate(close_bboxes):
        color = colors(idx)
        
        # OCR bounding box
        rect_ocr = patches.Rectangle((ocr_bbox[0], ocr_bbox[1]), ocr_bbox[2] - ocr_bbox[0], ocr_bbox[3] - ocr_bbox[1], linewidth=2, edgecolor=color, facecolor='none', label=f'OCR {idx}')
        ax.add_patch(rect_ocr)
        
        # Segmentation bounding box
        rect_seg = patches.Rectangle((seg_bbox[0], seg_bbox[1]), seg_bbox[2] - seg_bbox[0], seg_bbox[3] - seg_bbox[1], linewidth=2, edgecolor=color, facecolor='none', linestyle='dashed', label=f'Seg {idx}')
        ax.add_patch(rect_seg)

    # Add legend
    handles, labels = ax.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    # ax.legend(by_label.values(), by_label.keys())

    # # Show the plot
    # plt.show()
    # save the plot
    fig.savefig(f'{output_dir}/bounding_boxes_{image_name}.png')

    # %%
    # sort masks in the same order as close_bboxes 
    sorted_masks = []
    for idx, _ in close_bboxes_indices:
        sorted_masks.append(masks[idx])


    # %%
    cropped_image_list = process_images_and_use_sorted_mask(image, sorted_masks)

    # %%
    # now lets output the images to ../data/interrim/Exp8-CBS-080724 , 
    # the name of the image will be the same as the original image with the index of the sorted_masks appended to it
    # the name also must include the number of the ocr bounding box that is close to the segmentation bounding box
    # the image will be saved as a jpg file
    for idx, image_segment in enumerate( cropped_image_list) :
        index_segmetation , index_ocr = close_bboxes_indices[idx]
        pred_text = text_list[index_ocr]
        image_segment = cv2.cvtColor(image_segment, cv2.COLOR_BGR2RGB)
        cv2.imwrite(f'{output_dir}/image_index_{index_ocr}_{index_segmetation}_tag_{pred_text}.jpg',image_segment)

    # %%
    # find the segmentation bounding boxes that do not have a paired OCR bounding box
    unpaired_seg_indices = find_unpaired_seg_bboxes(seg_bboxes, close_bboxes_indices)
    # get the images for the unpaired segmentation bounding boxes
    # check if there are any unpaired segmentation bounding boxes
    if len(unpaired_seg_indices) > 0:
        unpaired_images ,unpaired_images_coordinates = [],[]
        for idx in unpaired_seg_indices:
            x, y, width, height = seg_bboxes[idx]
            image_b, masked_pixels = background_to_black(image=image, index=idx , masks=masks)
            cropped_image = image_b[int(y):int(height), int(x):int(width)]
            unpaired_images.append(cropped_image)
            unpaired_images_coordinates.append((x, y, width, height))

        # sort the unpaired images coordinates by the y coordinate and then by the x coordinate, in ascending order
        # store the index of the sorted coordinates
        unpaired_images_coordinates = np.array(unpaired_images_coordinates)
        sorted_indices = np.lexsort((unpaired_images_coordinates[:,0], unpaired_images_coordinates[:,1]))

        # use the sorted indices to sort the unpaired images
        unpaired_images = [unpaired_images[idx] for idx in sorted_indices]


        # now lets output the images to ../data/interrim/ , lets add tag "unpaired" to the name of the image
        # the image will be saved as a jpg file
        for idx, image_segment in enumerate( unpaired_images) :
            image_segment = cv2.cvtColor(image_segment, cv2.COLOR_BGR2RGB)
            cv2.imwrite(f'{output_dir}/image_unpaired_{idx}.jpg',image_segment)
