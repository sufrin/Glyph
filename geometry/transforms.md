# Coordinate mappings under rotation

Consider an image rectangle of size \((w, h)\) rotated about its centre
by theta, inside a bounding rectangle of size \((bw, bh)\). The centre
of the bounding rectangle is displaced by \((dx, dy)\) from that of the
original rectangle. What coordinate  does \((x, y)\) in the bounding
rectangle correspond to in the image rectangle?

To determine the coordinates in the original image rectangle that correspond to \((x, y)\) in the bounding rectangle, you need to reverse the transformations that were applied:

1. **Translate**: Shift the point by \((dx, dy)\) to undo the displacement between the centers of the rectangles.
2. **Rotate**: Apply the inverse rotation \(-\theta\) to the point to align it with the orientation of the original rectangle.

Here's the step-by-step process:

### 1. Translate the point

The coordinates of the point after translation are:
\[
(x_t, y_t) = (x - dx, y - dy)
\]

### 2. Rotate the point

To undo the rotation, apply the inverse rotation matrix. The formula for rotating a point \((x_t, y_t)\) by an angle \(-\theta\) is:
\[
\begin{aligned}
x_r &= x_t \cos(-\theta) - y_t \sin(-\theta) \\
y_r &= x_t \sin(-\theta) + y_t \cos(-\theta)
\end{aligned}
\]

Using trigonometric identities (\(\cos(-\theta) = \cos(\theta)\) and \(\sin(-\theta) = -\sin(\theta)\)), this becomes:
\[
\begin{aligned}
x_r &= x_t \cos(\theta) + y_t \sin(\theta) \\
y_r &= -x_t \sin(\theta) + y_t \cos(\theta)
\end{aligned}
\]

### Combined Transformation

Substitute \(x_t = x - dx\) and \(y_t = y - dy\):
\[
\begin{aligned}
x_r &= (x - dx) \cos(\theta) + (y - dy) \sin(\theta) \\
y_r &= -(x - dx) \sin(\theta) + (y - dy) \cos(\theta)
\end{aligned}
\]

Thus, \((x_r, y_r)\) are the coordinates in the original image rectangle corresponding to \((x, y)\) in the bounding rectangle.

### Summary

Given:
- \((x, y)\): Point in the bounding rectangle
- \((dx, dy)\): Displacement between centers
- \(\theta\): Rotation angle
- \((w, h)\): Dimensions of the original rectangle
- \((bw, bh)\): Dimensions of the bounding rectangle

The coordinates in the image rectangle are:
\[
\begin{aligned}
x_r &= (x - dx) \cos(\theta) + (y - dy) \sin(\theta) \\
y_r &= -(x - dx) \sin(\theta) + (y - dy) \cos(\theta)
\end{aligned}
\]

# Expressed as an affine transform.

This can be expressed as an affine transformation. The process involves undoing the displacement of the center, reversing the rotation, and finally scaling coordinates from the bounding rectangle to the original image rectangle. Let’s derive the transformation step by step.

### Problem Setup

1. **Original rectangle (image rectangle)**: Size \((w, h)\), centered at \((0, 0)\), rotated by \(\theta\).
2. **Bounding rectangle**: Size \((bw, bh)\), centered at \((dx, dy)\).
3. A point \((x, y)\) in the **bounding rectangle** needs to be mapped to a point \((x', y')\) in the **image rectangle**.

### Affine Transformation Steps

1. **Translate the bounding rectangle's coordinates to the original center**:
   \[
   (x_t, y_t) = (x - dx, y - dy)
   \]
   This centers the bounding rectangle at the origin.

2. **Undo the rotation**:
   Rotate the point \((x_t, y_t)\) by \(-\theta\) to align it with the unrotated image rectangle:
   \[
   x_r = x_t \cos(-\theta) - y_t \sin(-\theta) = x_t \cos\theta + y_t \sin\theta
   \]
   \[
   y_r = x_t \sin(-\theta) + y_t \cos(-\theta) = -x_t \sin\theta + y_t \cos\theta
   \]

3. **Scale the coordinates to the image rectangle size**:
   Scale the coordinates from the bounding rectangle dimensions \((bw, bh)\) to the image rectangle dimensions \((w, h)\):
   \[
   x' = x_r \cdot \frac{w}{bw}, \quad y' = y_r \cdot \frac{h}{bh}
   \]

### Combined Affine Transformation

Combining all steps, the transformation can be expressed as:
\[
\begin{bmatrix}
x' \\
y' \\
1
\end{bmatrix}
=
\begin{bmatrix}
\frac{w}{bw} \cos\theta & \frac{w}{bw} \sin\theta & -\frac{w}{bw} (dx \cos\theta + dy \sin\theta) \\
-\frac{h}{bh} \sin\theta & \frac{h}{bh} \cos\theta & -\frac{h}{bh} (-dx \sin\theta + dy \cos\theta) \\
0 & 0 & 1
\end{bmatrix}
\begin{bmatrix}
x \\
y \\
1
\end{bmatrix}
\]

This matrix compactly represents the affine transformation mapping \((x, y)\) in the bounding rectangle to \((x', y')\) in the image rectangle.