#In this lesson, we will be layering information with ggplot2 in order to make excellent graphics.

str(mpg)

#Aesthetics


ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()


ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()


ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()


ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()

#Could we change some of these mappings?

#You can also change visual aspects within a geom - these are not aesthetics because they do not map to a property on the data

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

#GEOMS


ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()


ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_smooth()


ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth(aes(linetype = drv))


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = FALSE)

#Can also separate out layers
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

#Take a moment to think about what this one is doing
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    shape = "circle open", size = 3, color = "red"
  )

#STATS

?geom_bar

#Can alter the stat in the geom or using after_stat

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

ggplot(diamonds) + 
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

?stat_summary

#FACETS

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free")

#POSITION

# Left
ggplot(mpg, aes(x = drv, color = drv)) + 
  geom_bar()

# Right
ggplot(mpg, aes(x = drv, fill = drv)) + 
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")

ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(position = "jitter")

#Can also use alpha here for large datasets



