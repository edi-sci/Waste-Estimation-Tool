# Waste-Estimation-Tool
The fusion of data exploration and machine learning, unraveling the intricacies of waste management through a comprehensive analysis and modeling approach.

# Unlocking the Secrets of Construction Waste: A Data-Driven Journey

## Introduction

Construction projects generate massive amounts of waste, contributing to environmental concerns and costs. By leveraging data analytics, we embarked on a journey to uncover patterns, develop predictive models, and ultimately, make informed decisions to mitigate waste. This blog post chronicles our exploration, from exploratory data analysis (EDA) to the development of a user-friendly predictive tool.

## Exploratory Data Analysis (EDA): Unveiling Patterns

Our journey began with a comprehensive EDA to understand the intricate relationships within our construction waste dataset. We delved into various aspects, including working days, floor area, bathrooms, cladding types, and more. Here are some key insights:

### Insights from EDA:

1. **Working Days Matter:**
   - A positive correlation between working days and waste volume suggests that longer construction periods are associated with increased waste generation.
2. **Cladding Considerations:**
   - Certain cladding materials, such as sheet and board cladding, exhibited varying impacts on waste volume.
3. **Spatial Dynamics:**
   - The presence of corners in a construction project positively influenced waste volume, indicating potential inefficiencies in corner areas.

## Predictive Modeling: Crafting the Crystal Ball

Armed with insights from our EDA, we moved on to predictive modeling using the Generalized Linear Model (GLM). We developed separate models for projects with and without roofs, recognizing the distinctive factors influencing waste generation in these scenarios.

### Model for Projects without Roofs:

**Key Findings:**
- Longer working days and specific cladding types play significant roles in waste volume.
- The model achieved a 78% accuracy in predicting waste volume within a certain range.

### Model for Projects with Roofs:

**Key Findings:**
- Principal components, roof cladding, and other factors contribute to waste generation in projects with roofs.
- This model demonstrated an even higher accuracy of 86%, showcasing its effectiveness in predicting waste volume.

## Building the App: Bringing Predictions to Life

With our predictive models in hand, we sought to make this valuable information accessible. We developed an interactive Shiny web application, enabling users to input project details and receive predictions for waste volume.

**MongoDB Connection Details:**
- We integrated MongoDB to enhance the app's scalability and data storage capabilities. The connection facilitated real-time updates and seamless data management.

**App Development Highlights:**
- MongoDB collections were utilized to store and retrieve data dynamically.
- The Shiny app incorporated user-friendly interfaces for input and result display, ensuring a seamless experience.

## The Grand Finale: Integrating Models for Comprehensive Insights

To maximize the accuracy and applicability of our predictions, we integrated the models for projects with and without roofs. This comprehensive approach yielded a powerful tool for stakeholders to make informed decisions.

**Final Output:**
- Our integrated model achieved a 100% accuracy in predicting waste volumes within the specified range.

## Conclusion: Empowering Sustainable Construction Practices

Our data-driven journey not only unveiled patterns in construction waste generation but also empowered stakeholders to make informed decisions. From insightful EDA to accurate predictive models and an interactive app, we've harnessed the power of data to drive sustainable construction practices.

Whether you're a project manager aiming to optimize resources or an environmentalist advocating for reduced waste, our journey demonstrates the transformative impact of data analytics on the construction industry. Together, let's build a future where construction is not just about structures but about sustainability and efficiency.
