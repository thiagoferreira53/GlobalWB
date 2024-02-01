#!/usr/local/bin/python3


# Name:        Era5_WTH
# Purpose:     Script to extract era5 wheater data
#
# Author:      Thiago Berton Ferreira
#              thiago.bertonf@gmail.com
# Created:     05/29/2020
# Updated:     07/22/2020         Added arguments, multiple requests and NC merge

import xarray #pip install toolz, pip install dask
import argparse
import cdsapi
import os



parser=argparse.ArgumentParser()
parser.add_argument('-N',
                    type=str,
                    help="North point")
parser.add_argument('-S',
                    type=str,
                    help="South point")
parser.add_argument('-W',
                    type=str,
                    help="West point")
parser.add_argument('-E',
                    type=str,
                    help="East point")
parser.add_argument('-YS',
                    type=str,
                    help="Start year")
parser.add_argument('-YE',
                    type=str,
                    help="End year")
args=parser.parse_args()

north_point = args.N
west_point = args.W
south_point = args.S
east_point = args.E
year_start = int(args.YS)
year_end = int(args.YE)

outputPath= "/Users/thiagoferreira53/PycharmProjects/DSSAT-Reader/ERA5_Data"
outputFolder = north_point+"N_"+south_point+"S_"+west_point+"W_"+east_point+"E"
output = outputPath+"/"+outputFolder

if not os.path.exists(output):
    os.mkdir(output)

c = cdsapi.Client(url="https://cds.climate.copernicus.eu/api/v2",key="44171:cd89f9d6-1035-4a1e-8702-4a522396bbaa")

for year in range(year_start,year_end):
    fileName = str(year) + "_hourly_data.nc"

    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'format': 'netcdf',
            'area': [
                north_point + "/" + west_point + "/" + south_point + "/" + east_point
            ],
            'variable': [
                '2m_dewpoint_temperature', '2m_temperature', #'100m_u_component_of_wind',
                'surface_net_solar_radiation', 'total_precipitation'
            ],
            'year': year,
            'day': [
                '01'#, '02', '03',
                #'04', '05', '06',
                #'07', '08', '09',
                #'10', '11', '12',
                #'13', '14', '15',
                #'16', '17', '18',
                #'19', '20', '21',
                #'22', '23', '24',
                #'25', '26', '27',
                #'28', '29', '30',
                #'31',
            ],
            'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ],
            'month': [
                '01'#,'02','03','04',
                #'05','06','07','08',
                #'09','10','11','12'
            ],
        },
        output+"/"+fileName)

ds = xarray.open_mfdataset(output+'/*_hourly_data.nc',combine = 'by_coords', concat_dim="time")

ds.to_netcdf(output+'/nc_combined.nc')

nc_combined = output+'/nc_combined.nc'
#nc = netCDF4.Dataset(nc_combined, mode='r')

#d = {}

#Write hourly data
#for i in nc.variables.keys():
#    d[i] = nc.variables[i][:]
#    if i == "time":
#        time_var = nc.variables['time']
#        d["time"] = netCDF4.num2date(time_var[:], time_var.units,calendar='julian')

#weather = open(output+'/weather.csv', 'w')
#write_weather = csv.writer(weather, delimiter=',')
#write_weather.writerow(["DATE", "LAT", "LNG", "SRAD", "T2M", "RAIN", "WIND"])
#with weather:
#    for r in range(len(d["latitude"])):
#        for c in range(len(d["longitude"])):
#            for e in range(d["t2m"].shape[0]):
#                write_weather.writerow((d["time"][e], d["latitude"][r],d["longitude"][c], round(d["ssr"][e,r,c],1), round(pytemperature.k2c(d["t2m"][e,r,c]),1), round(d["tp"][e,r,c]*1000,1),round(d["u100"][e,r,c],1)))

