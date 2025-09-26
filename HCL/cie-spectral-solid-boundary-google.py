
# Google search AI response

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from colour import (
    SpectralDistribution,
    sd_to_XYZ,
    XYZ_to_Luv,
    Luv_to_uv,
    SDS_ILLUMINANTS,
    MSDS_CMFS,
    SPECTRAL_SHAPE_DEFAULT,
)
from colour.plotting import (
    override_style,
    artist,
    render,
)

# 1. Define constants and a standard observer.
# The `CIE 1931 2 Degree Standard Observer` is commonly used for these calculations.
cmfs = MSDS_CMFS["CIE 1931 2 Degree Standard Observer"]
illuminant = SDS_ILLUMINANTS['D65']
spectral_shape = SPECTRAL_SHAPE_DEFAULT

# 2. Generate spectral distributions for the "optimal color solid".
# This function creates square-wave-like spectral distributions to define the boundary.
def generate_square_waves(samples=128):
    """
    Generate square-wave-like spectral distributions for approximating the optimal
    colour solid's outer surface.
    """
    waves = np.linspace(spectral_shape.start, spectral_shape.end, samples)
    step = (spectral_shape.end - spectral_shape.start) / samples
    
    square_waves = []
    for i in range(samples + 1):
        for j in range(i, samples + 1):
            wave = np.zeros(samples)
            if i < samples:
                wave[i:j] = 1.0
            square_waves.append(wave)

    return np.vstack(square_waves)

# 3. Calculate the XYZ values for the spectral solid's surface.
print("Generating spectral distributions and converting to XYZ...")
square_wave_sd = generate_square_waves(samples=128)
XYZ_surface = []
wavelengths = np.linspace(
    spectral_shape.start, spectral_shape.end, square_wave_sd.shape[1]
)
for wave_data in square_wave_sd:
    spd = SpectralDistribution(wave_data, wavelengths).align(spectral_shape)
    XYZ_surface.append(sd_to_XYZ(spd, illuminant=illuminant))
 
XYZ_surface = np.array(XYZ_surface)

# 4. Convert XYZ values to CIE L*u*v*.
print("Converting XYZ to L*u*v*...")
Luv_surface = XYZ_to_Luv(XYZ_surface)

# Convert to DataFrame
df = pd.DataFrame(Luv_surface)

df.to_csv("cie_luv_spectral_boundary_google.csv", index=False)

