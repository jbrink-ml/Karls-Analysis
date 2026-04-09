
library(ggplot2)

# Define the custom color palette from the image
custom_colors <- c("#00678a", "#b55300", "#9ECAE1") 

# Define a custom theme that matches the image's clean grid layout
my_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(colour = "gray80", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


# Plot 1: Age vs AF
ggplot(data, aes(x = age, y = AF)) +
  geom_point(color = "black", size = 1.5) +
  my_theme +
  labs(x = "Age", y = "AF")


# Plot 2: Sex vs AF
ggplot(data, aes(x = sex, y = AF, fill = sex)) +
  geom_boxplot(color = "black", alpha = 0.9) +
  scale_fill_manual(values = custom_colors) +
  my_theme +
  labs(x = "Sex", y = "AF")

# Plot 3: Treatment vs AF
ggplot(data, aes(x = treatment, y = AF, fill = treatment)) +
  geom_boxplot(color = "black", alpha = 0.9) +
  scale_fill_manual(values = custom_colors) +
  my_theme +
  labs(x = "Treatment", y = "AF")

# Plot 4: Sex vs PR
ggplot(data, aes(x = sex, fill = PR)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = custom_colors) +
  my_theme +
  labs(x = "Sex", y = "Count") 

# Plot 5: Treatment vs PR
ggplot(data, aes(x = treatment, fill = PR)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = custom_colors) +
  my_theme +
  labs(x = "Treatment", y = "Count")

# Plot 6: Age vs PR 
# Convert PR to a factor just to be safe, then get its names for the y-axis labels
pr_levels <- levels(as.factor(data$PR))

ggplot(data, aes(x = age, y = as.numeric(as.factor(PR)) - 1)) +
  # Add the points, jittered slightly up and down
  geom_jitter(aes(color = PR), width = 0, height = 0.05, size = 1.5) +
  # Add the logistic regression curve
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, color = "black", linewidth = 1) +
  # Manually set the y-axis back to the category names instead of 0 and 1
  scale_y_continuous(breaks = c(0, 1), labels = pr_levels) +
  scale_color_manual(values = custom_colors) +
  my_theme +
  labs(x = "Age", y = "PR")
