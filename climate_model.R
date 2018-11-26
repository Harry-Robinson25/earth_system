print('starting!')

# constants
S0 = 1370
sigma = 5.67e-8
Tinit = 14.0 + 273.15
N = 11
# sensitivity of temperature to CO2, in Kelvin per 1ppmv of CO2 change
T_CO2_sens = 0.005
# sensitivity of albedo to temperature, albedo increase per 1
# degree temperature change
a_T_sens = -0.005
#sensitivity of dust levels to changes in sea level
# - each 1m increase in sea level dust ^ by 0.01
dust_sea_sens = 0.01
#sensitivity of sea level to increase in temperature
# - each degree of warming causes a 0.1m increase
sea_T_sens = 0.1

#initialising vectors
year = numeric(N)
CO2 = numeric(N)
Talbedo = numeric(N)
Tgg = numeric(N)
Tearth = numeric(N)
TearthC = numeric(N)
albedo = numeric(N)
#adding the output of dust, initialised at 0
dust = numeric(N)
#now for sea level
sea = numeric(N)

# initial conditions for the year 2000
# set first year to 2000
year[1] = 2000 
# set first CO2 to 369 ppmv:
CO2[1] = 369
# for albedo, we need initial conditions for year 2000 and 2001, because of the lag:
albedo[1]=0.3
albedo[2]=0.3
# calculate the first temperature of the Earth with no
# greenhouse gases:
Talbedo[1] = (S0*(1-albedo[1])/(4*sigma))^(1/4)
# calculate the initial greenhouse warming, actual temperature,
# and convert to Celsius:
Tgg[1] = Tinit - Talbedo[1]
Tearth[1] = Talbedo[1] + Tgg[1] 
TearthC[1] = Tearth[1] - 273.15 
dust[1] = 0
sea[1] = 0



# Now loop forward in time
# We have the initial values now (index = 1), so we need to start
# at index 2.  So, i will loop through from 2 to N (the final
# year):

for (i in 2:N) {
  
  # to calculate this year, just add 1 to the previous
  # year:
  year[i] = year[i-1] + 1
  # to calculate this CO2, just multiply the previous value
  # by 1.01 (i.e. increase it by 1%):
  CO2[i] = CO2[i - 1] * 1.01
  # Calculate albedo for this year:
  # only do this bit if we are beyond the first two years:    
  if (i>2) albedo[i] = albedo[i - 1] + ((Tearth[i - 1] - Tearth[i - 2]) * a_T_sens)
  # Calculate temperature for this year (assuming no CO2):
  Talbedo[i] = (S0*(1-albedo[i])/(4*sigma))^(1/4)
  # This is the slightly complex bit....
  # The greenhouse warming for this year is equal to the
  # greenhouse warming for the previous year, plus some 
  # extra warming because CO2 has increased since then.
  # The amount of extra warming is equal to the amount that
  # CO2 has increased by, multiplied by T_CO2_sens: 
  Tgg[i] = Tgg[i-1] + T_CO2_sens * (CO2[i] - CO2[i - 1])
  # as before, work out actual the temperature of the Earth
  # in Kelvn and Celsius: 
  Tearth[i] = Talbedo[i] + Tgg[i]
  TearthC[i] = Tearth[i] - 273.15 
  #adding in the relationship between sea level and dust
  dust[i] = dust_sea_sens * (sea[i] - sea[i - 1])
  #adding the sea level/warming relationship
  sea[i] = sea_T_sens * (TearthC[i] - TearthC[i - 1])
  # ...and finally end the loop with a curly bracket...
}

# Make a plot! 
plot(year,TearthC,type="l",ylab="Temp",xlab="Year",main="temperature from 2000 to 2010")

# Stick it all in a dataframe so you can visualise it:
mydata = data.frame(year,CO2,albedo,Tgg,Talbedo,Tearth,TearthC,dust,sea)