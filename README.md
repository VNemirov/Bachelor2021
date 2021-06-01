# ReadME
Ved at køre scriptet 
  
  0. Loading packages and library.R

igennem det tilhørende R project bliver alle scripts og funktioner sourcet.
Output/grafer gemmes i mappen ./Output/Plots

I mappen ./Scripts/funktioner ligger alle definerede funktioner i egen fil.

Forklaring af hvert script:

1. S&P Russel priser.R  
Henter S&P500 og Russel2000 indeks priser fra Yahoo, og producerer grafik af deres udvikling.

2. Fordeling af inkrementer.R  
  Udregner log incrementer og plotter og tester normalitet ved Shapiro-Wilks test.

3. Lukkede MLE.R  
  Udregner MLE'er

4. Aktie simulation.R  
  Simulerer aktien (hvilken overaskelse) ved brug af definerede GBM funktioner.

5. Monte Carlo.R  
  Grafikker af Monte Carlo estimator konvergens for standard og antitetiske variater

6. CallPut.R  
  Forskellige grafikker af call option relationer samt Monte Carlo estimerede priser.

7. Greeks.R  
  Grafikker for de forskellige greeks for en europæisk call option.

8. Implicit Volatilitet.R  
  Hentede d. 26/5-2021 SPX og SPY optionspriser og udregnede implicite volatiliter for call og put optioner med udløb d. 25/6-2021 (1 måned) for at undgå dividender.

9. Delta Afdækning.R  
  Forskellige Delta afdækninger samt grafer med konvergens af afdækningsfejl.

10. Wilmott.R  
Udfører Wilmotts afdækningseksperiment ved brug FToDT og ser på PnL.

R Version og platform:
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          4                           
minor          0.5                         
year           2021                        
month          03                          
day            31                          
svn rev        80133                       
language       R                           
version.string R version 4.0.5 (2021-03-31)
nickname       Shake and Throw    
