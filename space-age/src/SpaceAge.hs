module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYearDurationInSecond :: Float
earthYearDurationInSecond        = 31557600

mercuryOrbitalPeriodInEarthYears :: Float
mercuryOrbitalPeriodInEarthYears = 0.2408467

venusOrbitalPeriodInEarthYears :: Float
venusOrbitalPeriodInEarthYears   = 0.61519726

marsOrbitalPeriodInEarthYears :: Float
marsOrbitalPeriodInEarthYears    = 1.8808158

jupiterOrbitalPeriodInEarthYears :: Float
jupiterOrbitalPeriodInEarthYears = 11.862615

saturnOrbitalPeriodInEarthYears :: Float
saturnOrbitalPeriodInEarthYears  = 29.447498

uranusOrbitalPeriodInEarthYears :: Float
uranusOrbitalPeriodInEarthYears  = 84.016846

neptuneOrbitalPeriodInEarthYears :: Float
neptuneOrbitalPeriodInEarthYears = 164.79132

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds / earthYearDurationInSecond * (1 / mercuryOrbitalPeriodInEarthYears)
ageOn Venus   seconds = seconds / earthYearDurationInSecond * (1 / venusOrbitalPeriodInEarthYears  )
ageOn Earth   seconds = seconds / earthYearDurationInSecond
ageOn Mars    seconds = seconds / earthYearDurationInSecond * (1 / marsOrbitalPeriodInEarthYears   )
ageOn Jupiter seconds = seconds / earthYearDurationInSecond * (1 / jupiterOrbitalPeriodInEarthYears)
ageOn Saturn  seconds = seconds / earthYearDurationInSecond * (1 / saturnOrbitalPeriodInEarthYears )
ageOn Uranus  seconds = seconds / earthYearDurationInSecond * (1 / uranusOrbitalPeriodInEarthYears )
ageOn Neptune seconds = seconds / earthYearDurationInSecond * (1 / neptuneOrbitalPeriodInEarthYears)
