import os
import glob
import pandas as pd
import plotly.express as px
import seaborn as sns
import numpy as np
from pypdf import PdfWriter

#ensure output folder exists
os.makedirs("02_visuals", exist_ok=True)

# (csv_path, output_pdf, tick_dates)
WINDOWS = [
    ("01_data/temp/dinard_long_for_python_1st_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part1.pdf",
     ["2013-10-19", "2013-11-03"]),
    ("01_data/temp/dinard_long_for_python_2nd_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part2.pdf",
     ["2013-11-17"]),
    ("01_data/temp/dinard_long_for_python_3rd_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part3.pdf",
     ["2013-12-03"]),
    ("01_data/temp/dinard_long_for_python_4th_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part4.pdf",
     ["2013-12-17"]),
    ("01_data/temp/dinard_long_for_python_5th_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part5.pdf",
     ["2014-01-01"]),
    ("01_data/temp/dinard_long_for_python_6th_part.csv",
     "02_visuals/Dinard_daylight_spectrum_3D_part6.pdf",
     ["2014-01-16", "2014-01-30"]),
]

#define the coolwarm color scale from seaborn
color_scale = sns.color_palette("coolwarm", as_cmap=True)

#sample colors from the colormap and convert to Plotly color scale
n_colors = 256
color_scale_sampled = color_scale(np.linspace(0, 1, n_colors))
plotly_color_scale = [
    [i / (n_colors - 1), f'rgb({int(color[0] * 255)},{int(color[1] * 255)},{int(color[2] * 255)})']
    for i, color in enumerate(color_scale_sampled)
]

def run_one(csv_path, out_pdf, tick_dates):
    #load data from CSV
    dinard_long = pd.read_csv(csv_path)

    #convert the 'time_date_UTC' column to datetime
    dinard_long['time_date_UTC'] = pd.to_datetime(dinard_long['time_date_UTC'])

    #sort the data by 'time_date_UTC' in descending order
    dinard_long = dinard_long.sort_values(by='time_date_UTC', ascending=False)

    #plotting with plotly
    fig = px.scatter_3d(
        dinard_long, x='time_date_UTC', y='wavelength', z='intensity',
        color='intensity', size_max=2, opacity=0.7,
        range_y=[380, 750], range_z=[0, 150],
        range_color=[0, 142],
        color_continuous_scale=plotly_color_scale,
        labels={'time_date_UTC': 'Time', 'wavelength': 'Wavelength (nm)', 'intensity': 'Light Intensity'}
    )

    #flip the x-axis
    time_range = [dinard_long['time_date_UTC'].min(), dinard_long['time_date_UTC'].max()]

    #define the x-axis tick values
    tickvals = pd.to_datetime(tick_dates)
    ticktext = tick_dates

    #fig 
    fig.update_layout(
        scene=dict(
            domain=dict(x=[0.06, 0.98], y=[0.06, 0.98]),
            xaxis=dict(
                title=dict(text='Time'),
                tickvals=tickvals,
                ticktext=ticktext,
                range=time_range[::-1],  #reverse axis
                backgroundcolor='rgba(0,0,0,0)',
                gridcolor='lightgray',
                showbackground=True,
                showgrid=True,
                tickangle=0,
                tickfont=dict(size=10),
                title_font=dict(size=14)
            ),
            yaxis=dict(
                title=dict(text='Wavelength (nm)'),
                range=[380, 750],
                backgroundcolor='rgba(0,0,0,0)',
                gridcolor='lightgray',
                showbackground=True,
                showgrid=False,
                zeroline=False,
                showline=False,
                tickfont=dict(size=10),
                title_font=dict(size=14)
            ),
            zaxis=dict(
                title=dict(text='Light Intensity<br>mW m<sup>-2</sup> nm<sup>-1</sup>'),
                range=[0, 150],
                backgroundcolor='rgba(0,0,0,0)',
                gridcolor='lightgray',
                showbackground=True,
                showgrid=False,
                zeroline=False,
                showline=False,
                tickfont=dict(size=10),
                title_font=dict(size=14)
            ),
            aspectmode='manual',
            aspectratio=dict(x=1.5, y=1, z=1),
            camera=dict(
                eye=dict(x=1, y=1.5, z=1.5),
                projection=dict(type='orthographic')
            )
        ),
        margin=dict(l=90, r=70, b=90, t=70), #changed margin here, for the ms I changed it manually in affinity designer
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        legend=dict(font=dict(size=10), itemsizing='constant')
    )

    #save to vector PDF - NEW (I saved them as png before, pdf now to compile files)
    fig.write_image(out_pdf, format='pdf', engine="kaleido", width=1200, height=900, scale=1)
    print(f"Saved {out_pdf}")

if __name__ == "__main__":
    # 1)generate all PDFs
    for csv_path, out_pdf, ticks in WINDOWS:
        run_one(csv_path, out_pdf, ticks)

    # 2)merge PDFs 
    merged_pdf = "02_visuals/Dinard_daylight_spectrum_all.pdf"
    part_pdfs = sorted(glob.glob(os.path.join("02_visuals", "Dinard_daylight_spectrum_3D_part*.pdf")))
    if part_pdfs:
        writer = PdfWriter()
        for pdf in part_pdfs:
            writer.append(pdf)  #append entire document
        with open(merged_pdf, "wb") as f:
            writer.write(f)
        print(f"Merged PDF saved to {merged_pdf}")
    else:
        print("No per-part PDFs found to merge.")