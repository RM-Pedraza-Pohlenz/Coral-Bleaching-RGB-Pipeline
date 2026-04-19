import numpy as np
import torch
import torchvision
import matplotlib.pyplot as plt
import cv2
import os 
from glob import glob 
from sklearn.cluster import KMeans
from collections import Counter
from skimage.color import rgb2lab, deltaE_cie76
import pandas as pd


def get_image(image_path):
    image = cv2.imread(image_path)
    # image = white_balance(img=image)
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    # image = preprocess_histograms( image=image)
    return image

## Extract and analyze colors
def RGB2HEX(color):
    return "#{:02x}{:02x}{:02x}".format(int(color[0]), int(color[1]), int(color[2]))

def drop_black_from_top_colors(top_colors_list):
    min_values = []
    for i in range(len(top_colors_list)):
        curr_color = rgb2lab(np.uint8(np.asarray([[top_colors_list[i]]])))
        diff = deltaE_cie76((0, 0, 0), curr_color)
        # print (diff, type(diff))
        min_values.append(diff[0][0])
        lowest_value_index = np.argmin(min_values) 
    top_colors_list.pop(lowest_value_index)
    return top_colors_list

def background_to_black ( image, index ):
    # Apply the mask to the image
    masked_img = image.copy()
    # masked_pixels = masked_img[masks[index]['segmentation']==True]
    # masked_img[masks[index]['segmentation']==False] = (0, 0, 0)  # Set masked pixels to black
    masked_pixels = masked_img[index['segmentation']==True]
    masked_img[index['segmentation']==False] = (0, 0, 0)  # Set masked pixels to black
    return masked_img ,masked_pixels


def get_color_distance(color_1, color_2):
    color_1_lab = rgb2lab(np.uint8(color_1))
    color_2_lab = rgb2lab(np.uint8(color_2))

    distances = deltaE_cie76(color_1_lab, color_2_lab)
    return distances


def closest_color(pixel, palette, palette_keys, color_map_RGB):
    pixel_lab = rgb2lab(np.uint8(pixel))
    distances = deltaE_cie76(pixel_lab, palette)
    closest_index = np.argmin(distances)
    return color_map_RGB[palette_keys[closest_index]]


def map_color_to_pixels(image):
    color_map_RGB = {'Black':(0,0,0),'White':(255,255,255),'B1': (247, 248, 232),'B2': (243, 244, 192),'B3': (234, 235, 137),'B4': (200, 206, 57),'B5': (148, 157, 56),
                    'B6': (92, 116, 52),'C1': (247, 235, 232),'C2': (246, 201, 192),'C3': (240, 156, 136),'C4': (207, 90, 58),'C5': (155, 50, 32),'C6': (101, 27, 13),
                    'D1': (246, 235, 224),'D2': (246, 219, 191),'D3': (239, 188, 135),'D4': (211, 147, 78),'D5': (151, 89, 36),'D6': (106, 58, 22),'E1': (247, 242, 227),
                    'E2': (246, 232, 191),'E3': (240, 213, 136),'E4': (209, 174, 68),'E5': (155, 124, 45),'E6': (111, 85, 34)}
    
    # Convert the colors in the color map to LAB space
    palette = np.array([rgb2lab(np.uint8(np.asarray(color_map_RGB[key]))) for key in color_map_RGB.keys()])
    palette_keys = list(color_map_RGB.keys())

    # Function to apply to each pixel
    func = lambda pixel: closest_color(pixel, palette, palette_keys, color_map_RGB)

    # Apply the function to each pixel
    mapped_img = np.apply_along_axis(func, -1, image)

    return mapped_img ,color_map_RGB



def count_pixel_colors(image, color_map_RGB):
  """
  Counts the number of pixels of each color in an image.

  Args:
    image: A NumPy array representing the image.
    color_map_RGB: A dictionary mapping color names to RGB tuples.

  Returns:
    A dictionary mapping color names to the number of pixels of that color in the image.
  """
  # Flatten the image into a 1D array
  # image_flat = image.flatten()
  # return image_flat
  reverse_dict = { value : key for key , value in color_map_RGB.items() }  


  # iterate over the image pixels
  all_pixels_list =[]
  for i in range(image.shape[0]):
      for j in range(image.shape[1]):
        pixel = image[i, j]  
        # discard black 
        # if reverse_dict[str(pixel)] != 'Black':
        all_pixels_list.append(pixel)

  # # Count the occurrences of each pixel value
  pixel_counts = Counter(tuple(pixel_1) for pixel_1 in all_pixels_list)
  # delete the black key from the dictionary 
  del pixel_counts[(0,0,0)] 

  # pass the values to a list 
  total_pixels = [ item for key , item in pixel_counts.items() if key != (0,0,0)]
  # sum all the values 
  total_pixels = np.sum(total_pixels)
  # # Count the number of pixels of each color in the color map
  color_counts = {color_name: pixel_counts.get(color_rgb, 0)/total_pixels * 100 for color_rgb,color_name in reverse_dict.items()}

  return pixel_counts, color_counts 

def plot_compare_mapped_image_save(img1_rgb,filename):

    # get the mapped image 
    mapped_image , color_map = map_color_to_pixels(image=img1_rgb )
    del color_map['Black'] 

    color_counts, reverse_dict = count_pixel_colors(image=mapped_image , color_map_RGB=color_map)
    # lists = sorted(reverse_dict.items()) # sorted by key, return a list of tuples
    lists = sorted(reverse_dict.items(), key=lambda kv: kv[1], reverse=True)
    # color_name, percentage_color_name = zip(*lists) # unpac the tupple
    color_name, percentage_color_name = [],[]
    for c , p in lists:
        if p > 1 :
            color_name.append(c)
            percentage_color_name.append(p)

    hex_colors_map = [RGB2HEX(color_map[key]) for key in color_name]

    results_df = pd.DataFrame({
    'color_name': color_name,
    'percentage_color_name': percentage_color_name,
    'hex_colors_map': hex_colors_map
    })
    result_name = filename.replace(".png",".csv")
    results_df.to_csv(result_name)



    # Create a figure and subplots
    fig, (ax1, ax2, ax3 ) = plt.subplots(nrows=1,ncols=3,figsize=(30, 10))  # Adjust figsize as needed
    plt.title(label=filename.split("/")[-1].split(".")[0])
    # Display the images
    ax1.imshow(img1_rgb)
    ax1.set_title("Original")
    ax1.axis('off') 

    ax2.imshow(mapped_image)
    ax2.set_title("Mapped Image")
    ax2.axis('off') 


    # ax2.set_ylabel("Mapped Image")
    # ax2.set_xlabel("Color code in chart")

    ax3.bar(color_name, percentage_color_name, color = hex_colors_map , edgecolor='black' )
    ax3.yaxis.grid(True, linestyle='--', which='major',color='grey', alpha=.25)
    ax3.set_xlabel("Color code in chart")
    ax3.set_ylabel("Percentage of pixel on the image")
    plt.xticks(rotation=90)
    
    # plt.xlabel("Color code in chart")
    # plt.ylim(lower_y_limit,higher_y_limit)

    # Adjust spacing between subplots
    plt.tight_layout()
    #
    plt.savefig(fname=filename ,transparent=True ,format='jpg')
    # use close to dont show all images at once 
    # plt.close()

def get_colors(image, number_of_colors, show_chart):
    
    modified_image = image.reshape( image.shape[0]*image.shape[1],3  )

    # from modified_image filter out the black color
    modified_image = [pixel for pixel in modified_image if pixel[0] != 0 and pixel[1] != 0 and pixel[2] != 0]
        
    clf = KMeans(n_clusters = number_of_colors, n_init='auto', random_state=73)
    labels = clf.fit_predict(modified_image)
        
    counts = Counter(labels)
    # sort to ensure correct color percentage
    counts = dict(sorted(counts.items()))
    # print (counts)
    center_colors = clf.cluster_centers_

    # We get ordered colors by iterating through the keys
    ordered_colors = [center_colors[i] for i in counts.keys()]
    hex_colors = [RGB2HEX(ordered_colors[i]) for i in counts.keys()]
    rgb_colors = [ordered_colors[i] for i in counts.keys()]

    df_colors = pd.DataFrame({ #"ordered_colors":ordered_colors,
                "hex_colors":hex_colors,
                "counts_value":counts.values(),
                "rgb_colors":rgb_colors})
    # df_colors = df_colors[~df_colors['hex_colors'].str.contains("#000000")]
    df_colors['hex_colors'] = df_colors['hex_colors'].astype(str)

    df_colors['is_dark'] = df_colors['hex_colors'].apply(is_dark_color)

    # df_colors = df_colors[df_colors['is_dark'] == False]
        # print (df_colors.info())


    if (show_chart):
        plt.figure(figsize = (8, 6))
        plt.pie(df_colors["counts_value"], labels= df_colors["rgb_colors"], colors=df_colors["hex_colors"])
            # plt.pie(counts.values(), labels = rgb_colors, colors = hex_colors)
        
    return df_colors.drop("counts_value",axis=1)

def is_dark_color(hex_code):
    """
    Determines whether a given hex color code represents a dark color.

    Args:
        hex_code (str): The hex color code (e.g., '#FF0000').

    Returns:
        bool: True if the color is considered dark, False otherwise.
    """

    r, g, b = tuple(int(hex_code.lstrip('#')[i:i+2], 16) for i in (0, 2, 4))
    # Calculate a weighted average of the RGB components, considering human eye sensitivity
    luminosity = (0.299 * r + 0.587 * g + 0.114 * b) / 255

    # Threshold based on luminance and desired darkness level
    return luminosity < 0.1  # Adjust this threshold as needed

def is_cuda_available():
    """Checks if CUDA is available and can be used by PyTorch.

    Returns:
        bool: True if CUDA is available, False otherwise.
    """

    return torch.cuda.is_available()

def load_sam_model():
    ## Load the model for segmentation ( the SAM deep learning algorithm )
    from segment_anything import sam_model_registry, SamAutomaticMaskGenerator
    path_to_models = "/".join(os.getcwd().split("/")[:-1])

    model_type = "vit_l"


    sam_checkpoint = {'vit_h':f'{path_to_models}/models/sam_vit_h_4b8939.pth', 
                    'vit_l':f'{path_to_models}/models/sam_vit_l_0b3195.pth',
                    'vit_b':f'{path_to_models}/models/sam_vit_b_01ec64.pth'}

    if is_cuda_available():
        print("CUDA is available!")
        device = torch.device("cuda")
    else:
        print("CUDA is not available. Using CPU.")
        device = torch.device("cpu")
    # The following line is for the use of my second gpu wich is free
    # device = torch.device("cuda:1")

    sam = sam_model_registry[model_type](checkpoint=sam_checkpoint[model_type])
    sam.to(device=device)

    mask_generator = SamAutomaticMaskGenerator(sam)
    return mask_generator

def get_sorted_by_area(image, anns):
    area_list=[]
    cropped_image_dic ={}
    mask_number = [] 
    mask_pixles_dic = {}
    for i in range(len(anns)):
        x, y, width, height = anns[i]['bbox']
        area = anns[i]["area"]
        image_b, masked_pixels = background_to_black(image=image, index=anns[i])
        cropped_image = image_b[int(y):int(y+height), int(x):int(x+width)]

        area_list.append(area)
        cropped_image_dic[i] = cropped_image
        mask_pixles_dic[i] = masked_pixels
        mask_number.append(i)
    df = pd.DataFrame([area_list,mask_number])
    df = df.T
    df.columns = ['area','mask_number']
    df.sort_values(by='area', ascending=False, inplace=True)
    df.dropna(inplace=True)
    return df , cropped_image_dic , mask_pixles_dic

def process_images(image, masks):
    image_dataframe, cropped_image_list , mask_pixels_dict = get_sorted_by_area( image=image , anns=masks )
    top_six_img_by_area = image_dataframe['mask_number'].head(n=10).to_list()
    list_of_images = [ cropped_image_list [idx ] for idx in top_six_img_by_area  ]
    # titles = ['Image 1', 'Image 2', 'Image 3', 'Image 4', 'Image 5', 'Image 6','Image 7','Image 8','Image 9','Image 10']
    return list_of_images # , titles

def save_my_masked_image(list_of_images,name, path_absolute_to_output):
    for i,image in enumerate (list_of_images):
        filename = os.path.join(path_absolute_to_output,f"{name}_{str(i)}.jpg")
        # image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
        cv2.imwrite(filename, image)