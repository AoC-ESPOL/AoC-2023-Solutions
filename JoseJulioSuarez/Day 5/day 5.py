
#NO FUNCIONA CORRECTAMENTE

seeds = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\seeds.txt'
seedToSoil = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\seed-to-soil.txt'
soilToFertilizer = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\soil-to-fertilizer.txt'
fertilizerToWater = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\ertilizer-to-water.txt'
waterToLight = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\water-to-light.txt'
lightToTemperature = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\light-to-temperature.txt'
temperatureToHumidity = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\emperature-to-humidity.txt'
humidityToLocation = 'D:\ESPOL\AoC-2023-Solutions\JoseJulioSuarez\Day 5\humidity-to-location.txt'

seedslist =[]
with open(seeds) as file:
    for line in file:
        seedslist += line.split(' ')

seedToSoillist = []
with open(seedToSoil) as file:
    for line in file:
        seedToSoillist.append(line.split(' '))

soilToFertilizerlist = []
with open(soilToFertilizer) as file:
    for line in file:
        soilToFertilizerlist.append(line.split(' '))

fertilizerToWaterlist = []
with open(fertilizerToWater) as file:
    for line in file:
        fertilizerToWaterlist.append(line.split(' '))

waterToLightlist = []
with open(waterToLight) as file:
    for line in file:
        waterToLightlist.append(line.split(' '))

lightToTemperaturelist = []
with open(lightToTemperature) as file:
    for line in file:
        lightToTemperaturelist.append(line.split(' '))

temperatureToHumiditylist = []
with open(temperatureToHumidity) as file:
    for line in file:
        temperatureToHumiditylist.append(line.split(' '))

humidityToLocationlist = []
with open(humidityToLocation) as file:
    for line in file:
        humidityToLocationlist.append(line.split(' '))

entries = []
for seed in seedslist:
    for soil in seedToSoillist:
        seedFloor = int(soil[0])
        range = int(soil[2])
        seedCieling = seedFloor + range
        seedValue = int(seed)
        if seedFloor <= seedValue <= seedCieling:
            entry = [seedValue, int(soil[1])+seedValue-seedFloor, 0, 0, 0, 0, 0, 0]
            entries.append(entry)
            break

for entry in entries:
    soil = entry[1]
    for fertilizer in soilToFertilizerlist:
        soilFloor = int(fertilizer[0])
        range = int(fertilizer[2])
        soilCieling = soilFloor + range
        if soilFloor <= soil <= soilCieling:
            entry[2] = int(fertilizer[1]) + soil - soilFloor
            break

for entry in entries:
    fertilizer = entry[2]
    for water in fertilizerToWaterlist:
        fertilizerFloor = int(water[0])
        range = int(water[2])
        fertilizerCieling = fertilizerFloor + range
        if fertilizerFloor <= fertilizer <= fertilizerCieling:
            entry[3] = int(water[1]) + fertilizer - fertilizerFloor
            break

for entry in entries:
    water = entry[3]
    for light in waterToLightlist:
        waterFloor = int(light[0])
        range = int(light[2])
        waterCieling = waterFloor + range
        if waterFloor <= water <= waterCieling:
            entry[4] = int(light[1]) + water - waterFloor
            break

for entry in entries:
    light = entry[4]
    for temperature in lightToTemperaturelist:
        lightFloor = int(temperature[0])
        range = int(temperature[2])
        lightCieling = lightFloor + range
        if lightFloor <= light <= lightCieling:
            entry[5] = int(temperature[1]) + light - lightFloor
            break

for entry in entries:
    temperature = entry[5]
    for humidity in temperatureToHumiditylist:
        temperatureFloor = int(humidity[0])
        range = int(humidity[2])
        temperatureCieling = temperatureFloor + range
        if temperatureFloor <= temperature <= temperatureCieling:
            entry[6] = int(humidity[1]) + temperature - temperatureFloor
            break

for entry in entries:
    humidity = entry[6]
    for location in humidityToLocationlist:
        humidityFloor = int(location[0])
        range = int(location[2])
        humidityCieling = humidityFloor + range
        if humidityFloor <= humidity <= humidityCieling:
            entry[7] = int(location[1]) + humidity - humidityFloor
            break

sortinglist = []
for entry in entries:
    sortinglist.append(entry[7])
sortinglist.sort()

closestLocation = sortinglist[0]

print(entries)
print(closestLocation)
