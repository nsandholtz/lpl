library(sp)

# constants

hoop_x = 0
hoop_y = 5.25

# help functions
center = function(xy) {
  x = xy[, 1]
  y = xy[, 2]
  x = x - hoop_x
  y = y - hoop_y
  cbind(x, y)
}

uncenter = function(xy) {
  x = xy[, 1]
  y = xy[, 2]
  x = x + hoop_x
  y = y + hoop_y
  data.frame(x = x, y = y)
}

calc_angle <- function(x, cx, r) {
  acos((x - cx)/r)
}

calc_y_angle <- function(y, cy, r) {
  asin((y - cy)/r)
}

arc_coords <- function(x, y, r, from = 0, to = 2 * pi) {
  theta <- seq(from, to, length = 100)
  data.frame(x = x + r * cos(theta), y = y + r * sin(theta))
}

# define regions in functions

# right hoop
basis_10 = function(x = 25, y = 5.25, plot = FALSE) {
  
  outer_radius = 8
  from_outer = -1 * calc_y_angle(0, hoop_y, outer_radius) + pi
  to_outer = pi / 2
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  poly_df = uncenter(rbind(arc1, c(0, -5.25)))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis10")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

# left hoop
basis_11 = function(x = 25, y = 5.25, plot = FALSE) {
  
  outer_radius = 8
  from_outer = calc_y_angle(0, hoop_y, outer_radius)
  to_outer = pi / 2
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  poly_df = uncenter(rbind(arc1, c(0, -5.25)))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis11")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_20 = function(x = 25, y = 5.25, plot = FALSE) {
  
  corner_3_dist = 22
  outer_radius = 23.75
  inner_radius = 8
  
  outer_coords = data.frame(x = rep(-corner_3_dist, 2), y = c(-5.25, 8.75))
  from_outer = -1 * calc_y_angle(8.75, cy = 0, r = outer_radius) + pi
  to_outer = from_inner = 3 * pi / 4
  to_inner = -1 * calc_y_angle(0, hoop_y, inner_radius) + pi
  
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  poly_df = uncenter(rbind(outer_coords, arc1, arc2))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis30")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_21 = function(x = 25, y = 5.25, plot = FALSE) {
  
  outer_radius = 23.75
  inner_radius = 8
  
  from_outer = to_inner = 3 * pi / 4
  to_outer = from_inner = pi / 4
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  poly_df = uncenter(rbind(arc1, arc2))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis31")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_22 = function(x = 25, y = 5.25, plot = FALSE) {
  
  corner_3_dist = 22
  outer_radius = 23.75
  inner_radius = 8
  
  outer_coords = data.frame(x = rep(corner_3_dist, 2), y = c(8.75, -5.25))
  from_outer = to_inner = pi / 4
  to_outer = calc_y_angle(8.75, cy = 0, r = outer_radius)
  from_inner = calc_y_angle(0, hoop_y, inner_radius)
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  
  poly_df = uncenter(rbind(arc1, outer_coords, arc2))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis33")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_30 = function(x, y, plot = FALSE) {
  coords = data.frame(x = c(-22, -22, -25, -25),
                      y = c(-5.25, 8.75, 8.75, -5.25))
  poly_df = uncenter(coords)
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis40")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}


basis_31 = function(x, y, plot = FALSE) {
  inner_radius = 23.75
  outer_radius = 32
  
  arc_length = ((-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - 
    calc_y_angle(8.75, cy = 0, r = inner_radius)) / 3
  
  from_outer = -1 * calc_angle(25, 0, outer_radius) + pi
  to_outer = from_inner = (-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - arc_length
  to_inner = -1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi
  
  outer_coords = data.frame(x = -25, y = 8.75)
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  
  poly_df = uncenter(rbind(outer_coords, arc1, arc2))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis41")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_32 = function(x = 5.25, y = 25, plot = FALSE) {
  
  outer_radius = 32
  inner_radius = 23.75
  
  arc_length = ((-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - 
                  calc_y_angle(8.75, cy = 0, r = inner_radius)) / 3
  
  from_outer = to_inner = (-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - arc_length
  to_outer = from_inner = (-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - (2 * arc_length)
  
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  poly_df = uncenter(rbind(arc1, arc2))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis42")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_33 = function(x = 5.25, y = 25, plot = FALSE) {
  
  outer_radius = 32
  inner_radius = 23.75
  
  arc_length = ((-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - 
                  calc_y_angle(8.75, cy = 0, r = inner_radius)) / 3
  
  from_outer = to_inner = (-1 * calc_y_angle(8.75, cy = 0, r = inner_radius) + pi) - (2 * arc_length)
  to_outer = calc_angle(25, 0, outer_radius)
  from_inner = calc_y_angle(8.75, cy = 0, r = inner_radius) 

  outer_coords = data.frame(x = 25, y = 8.75)
  arc1 = arc_coords(0, 0, outer_radius, from = from_outer, to = to_outer)
  arc2 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  poly_df = uncenter(rbind(arc1, outer_coords, arc2))
  
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis43")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_34 = function(x, y, plot = FALSE) {
  coords = data.frame(x = c(22, 22, 25, 25),
                      y = c(-5.25, 8.75, 8.75, -5.25))
  poly_df = uncenter(coords)
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis45")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

basis_40 = function(x, y, plot = FALSE) {
  inner_radius = 32
  from_inner = calc_y_angle(25, cy = 0, r = inner_radius)
  to_inner = calc_y_angle(-25, cy = 0, r = inner_radius)
  
  arc1 = arc_coords(0, 0, inner_radius, from = from_inner, to = to_inner)
  corner_df = data.frame(x = rep(41.75, 2), y = c(-25, 25))
  
  poly_df = uncenter(rbind(corner_df, arc1))
  if (plot) {
    sp::Polygons(list(sp::Polygon(poly_df, hole = FALSE)), "basis50")
  } else {
    point.in.polygon(x, y, poly_df$x, poly_df$y)
  }
}

assign_region = function(x, y) {
  results = cbind(
    basis_10(x = x, y = y),
    basis_11(x = x, y = y),
    basis_20(x = x, y = y),
    basis_21(x = x, y = y),
    basis_22(x = x, y = y),
    basis_30(x = x, y = y),
    basis_31(x = x, y = y),
    basis_32(x = x, y = y),
    basis_33(x = x, y = y),
    basis_34(x = x, y = y),
    basis_40(x = x, y = y)
  )
  
  colnames(results) = c(paste0("basis_1", 0:1),
                        paste0("basis_2", 0:2),
                        paste0("basis_3", 0:4),
                        "basis_40")
  results
}

court_regions = function() {
  sp::SpatialPolygons(list(
     basis_10(plot = TRUE),
     basis_11(plot = TRUE),
     basis_20(plot = TRUE),
     basis_21(plot = TRUE),
     basis_22(plot = TRUE),
     basis_30(plot = TRUE),
     basis_31(plot = TRUE),
     basis_32(plot = TRUE),
     basis_33(plot = TRUE),
     basis_34(plot = TRUE)
  ))
}

plot(court_regions())

     