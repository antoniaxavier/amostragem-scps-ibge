# \# Sampling Design with Geographic Coordinates

# 

# Advanced methods for coordinate-based sample selection applied to the IBGE Master Sample.

# 

# \## Overview

# 

# This repository implements spatially-balanced sampling designs for official statistics, 

# with a focus on integrating geographic information into Primary Sampling Unit selection.

# 

# \## Methods

# 

# \- \*\*Estimand:\*\* unbiased estimates of population totals with reduced spatial autocorrelation

# \- \*\*Design:\*\* coordinate-based stratification and inclusion probability weighting

# \- \*\*Software:\*\* R with `sf`, `sampling`, and custom utilities

# \- \*\*Validation:\*\* simulation studies and cross-validation on known populations

# 

# Full technical details in `docs/methods.md`.

# 

# \## Stack

# 

# | Component | Tool |

# |-----------|------|

# | Language | R (Positron) |

# | Geospatial | sf, sp |

# | Sampling | sampling, survey |

# | Data | CSV (IBGE public data only) |

# | Reproducibility | renv |

# | Docs | Quarto / Markdown |

# 

# \## Data

# 

# This project uses \*\*only open data\*\* from IBGE 

# No restricted microdata or civil registry information is included.

# 

# \## Usage


