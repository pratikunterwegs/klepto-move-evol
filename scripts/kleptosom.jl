# try gigasom on kleptomove data
using CSV
using DataFrames
using GigaSOM
using Gadfly
using Statistics

using RCall

# load data
data = CSV.read("data_sim/temp_data.csv", DataFrame)
data_som = data[:,1:3]

# run gigasom
som = initGigaSOM(data_som, 3, 3)
som = trainGigaSOM(som, data_som)

som.codes

indices = mapToGigaSOM(som, data_som)

e = embedGigaSOM(som, data_som)

# rename data
data[:,"morph"] = indices.index

# plot
plot(
    x=e[:,1], y=e[:,2], color = data[:,1],
)

rename!(data, names(data) .=> ["nonhandlers","handlers","items","fitness","morph"])

# count morphs
combine(
    groupby(data, :morph),
    :fitness .=> [mean, std]
)

# other sort of plot
R"library(ggplot2)"
R"ggplot($data)+
    geom_point(aes(nonhandlers, handlers, col = factor(morph)))+
    scale_colour_viridis_d(option='H')"
    