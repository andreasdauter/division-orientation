import nibabel as nib
import numpy as np
import os
from glob import glob

# ---- user inputs ----
input_dir = r"C:/Users/andre/Documents/proliferation_masks_registered"
output_path = r"C:/Users/andre/Documents/proliferation_analysis/proliferation_atlas_e105.nii.gz"
# ---------------------

# Find all .nii files in the folder
nii_paths = sorted(glob(os.path.join(input_dir, "*.nii*")))

if len(nii_paths) == 0:
    raise ValueError("No .nii files found in the specified directory.")

print(f"Found {len(nii_paths)} NIfTI files.")

# Load images
images = []
affine = None
header = None
ref_shape = None

for i, path in enumerate(nii_paths):
    nii = nib.load(path)
    data = nii.get_fdata()

    if i == 0:
        affine = nii.affine
        header = nii.header
        ref_shape = data.shape
    else:
        if data.shape != ref_shape:
            raise ValueError(
                f"Shape mismatch: {path} has shape {data.shape}, expected {ref_shape}"
            )

    images.append(data)

# Stack into 4D array: (X, Y, Z, N)
stack = np.stack(images, axis=-1)

# Intersection of non-zero voxels across all images
intersection_mask = np.all(stack > 0, axis=-1)

# Initialize output volume
output = np.zeros(ref_shape, dtype=np.float32)

# Compute mean only in intersection
output[intersection_mask] = stack[intersection_mask].mean(axis=-1)

# Save result
out_nii = nib.Nifti1Image(output, affine, header)
nib.save(out_nii, output_path)

print(f"Saved averaged intersection image to: {output_path}")
