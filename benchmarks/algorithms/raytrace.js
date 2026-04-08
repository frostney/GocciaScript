/*---
description: Raytracer benchmarks — adapted from Octane 2.0 RayTrace benchmark
---*/

class Vec3 {
  constructor(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add(v) { return new Vec3(this.x + v.x, this.y + v.y, this.z + v.z); }
  sub(v) { return new Vec3(this.x - v.x, this.y - v.y, this.z - v.z); }
  scale(s) { return new Vec3(this.x * s, this.y * s, this.z * s); }
  dot(v) { return this.x * v.x + this.y * v.y + this.z * v.z; }
  length() { return Math.sqrt(this.dot(this)); }

  normalize() {
    const len = this.length();
    if (len === 0) { return new Vec3(0, 0, 0); }
    return this.scale(1 / len);
  }

  reflect(normal) {
    return this.sub(normal.scale(2 * this.dot(normal)));
  }
}

class Color {
  constructor(r, g, b) {
    this.r = r;
    this.g = g;
    this.b = b;
  }

  add(c) { return new Color(this.r + c.r, this.g + c.g, this.b + c.b); }
  scale(s) { return new Color(this.r * s, this.g * s, this.b * s); }
  multiply(c) { return new Color(this.r * c.r, this.g * c.g, this.b * c.b); }

  clamp() {
    const cl = (v) => v < 0 ? 0 : v > 1 ? 1 : v;
    return new Color(cl(this.r), cl(this.g), cl(this.b));
  }
}

class Ray {
  constructor(origin, direction) {
    this.origin = origin;
    this.direction = direction;
  }
}

class Material {
  constructor(color, ambient, diffuse, specular, shininess, reflectivity) {
    this.color = color;
    this.ambient = ambient;
    this.diffuse = diffuse;
    this.specular = specular;
    this.shininess = shininess;
    this.reflectivity = reflectivity;
  }
}

class Sphere {
  constructor(center, radius, material) {
    this.center = center;
    this.radius = radius;
    this.material = material;
  }

  intersect(ray) {
    const oc = ray.origin.sub(this.center);
    const a = ray.direction.dot(ray.direction);
    const b = 2 * oc.dot(ray.direction);
    const c = oc.dot(oc) - this.radius * this.radius;
    const discriminant = b * b - 4 * a * c;
    if (discriminant < 0) { return null; }
    const t = (-b - Math.sqrt(discriminant)) / (2 * a);
    if (t < 0.001) { return null; }
    return t;
  }

  normalAt(point) {
    return point.sub(this.center).normalize();
  }
}

class Plane {
  constructor(point, normal, material) {
    this.point = point;
    this.normal = normal.normalize();
    this.material = material;
  }

  intersect(ray) {
    const denom = this.normal.dot(ray.direction);
    if (Math.abs(denom) < 0.0001) { return null; }
    const t = this.point.sub(ray.origin).dot(this.normal) / denom;
    if (t < 0.001) { return null; }
    return t;
  }

  normalAt(point) {
    return this.normal;
  }
}

class Light {
  constructor(position, color, intensity) {
    this.position = position;
    this.color = color;
    this.intensity = intensity;
  }
}

class Scene {
  constructor(objects, lights, background) {
    this.objects = objects;
    this.lights = lights;
    this.background = background;
  }

  // Find closest intersection
  intersect(ray) {
    let closest = null;
    let minT = Infinity;
    for (const obj of this.objects) {
      const t = obj.intersect(ray);
      if (t !== null && t < minT) {
        minT = t;
        closest = { object: obj, t: t };
      }
    }
    return closest;
  }

  // Shade a point
  shade(ray, hit, depth) {
    const point = ray.origin.add(ray.direction.scale(hit.t));
    const normal = hit.object.normalAt(point);
    const material = hit.object.material;
    let color = material.color.scale(material.ambient);

    for (const light of this.lights) {
      const toLight = light.position.sub(point);
      const lightDir = toLight.normalize();

      // Shadow check
      const shadowRay = new Ray(point, lightDir);
      const shadowHit = this.intersect(shadowRay);
      const inShadow = shadowHit !== null && shadowHit.t < toLight.length();

      if (!inShadow) {
        // Diffuse
        const diff = Math.max(0, normal.dot(lightDir));
        const diffuseColor = material.color.scale(material.diffuse * diff * light.intensity);
        color = color.add(diffuseColor.multiply(light.color));

        // Specular (Phong)
        const reflected = lightDir.scale(-1).reflect(normal);
        const viewDir = ray.direction.scale(-1).normalize();
        const spec = Math.pow(Math.max(0, reflected.dot(viewDir)), material.shininess);
        const specColor = light.color.scale(material.specular * spec * light.intensity);
        color = color.add(specColor);
      }
    }

    // Reflection
    if (material.reflectivity > 0 && depth > 0) {
      const reflectDir = ray.direction.reflect(normal);
      const reflectRay = new Ray(point, reflectDir);
      const reflectColor = this.traceRay(reflectRay, depth - 1);
      color = color.add(reflectColor.scale(material.reflectivity));
    }

    return color.clamp();
  }

  traceRay(ray, depth) {
    const hit = this.intersect(ray);
    if (hit === null) { return this.background; }
    return this.shade(ray, hit, depth);
  }
}

const createScene = () => {
  const red = new Material(new Color(1, 0.2, 0.2), 0.1, 0.7, 0.5, 50, 0.3);
  const blue = new Material(new Color(0.2, 0.2, 1), 0.1, 0.7, 0.5, 50, 0.1);
  const green = new Material(new Color(0.2, 0.8, 0.2), 0.1, 0.6, 0.3, 30, 0.05);
  const gray = new Material(new Color(0.5, 0.5, 0.5), 0.1, 0.6, 0.2, 20, 0.1);

  const objects = [
    new Sphere(new Vec3(0, 0, -5), 1, red),
    new Sphere(new Vec3(2, 0.5, -4), 0.75, blue),
    new Sphere(new Vec3(-1.5, -0.5, -3.5), 0.5, green),
    new Plane(new Vec3(0, -1, 0), new Vec3(0, 1, 0), gray),
  ];

  const lights = [
    new Light(new Vec3(-5, 5, -2), new Color(1, 1, 1), 1.0),
    new Light(new Vec3(5, 3, -1), new Color(0.8, 0.8, 1), 0.6),
  ];

  return new Scene(objects, lights, new Color(0.1, 0.1, 0.15));
};

const render = (scene, width, height, maxDepth) => {
  const aspectRatio = width / height;
  const fov = 1.0; // Simple field of view factor
  const pixels = Array.from({ length: width * height }, (_, idx) => {
    const x = idx % width;
    const y = Math.floor(idx / width);
    // Map pixel to [-1, 1] range
    const px = (2 * (x + 0.5) / width - 1) * aspectRatio * fov;
    const py = (1 - 2 * (y + 0.5) / height) * fov;
    const ray = new Ray(new Vec3(0, 0, 0), new Vec3(px, py, -1).normalize());
    return scene.traceRay(ray, maxDepth);
  });
  return pixels;
};

suite("raytrace", () => {
  bench("intersection only (no shading)", {
    setup: () => createScene(),
    run: (scene) => {
      const width = 16;
      const height = 16;
      Array.from({ length: width * height }, (_, idx) => {
        const x = idx % width;
        const y = Math.floor(idx / width);
        const px = (2 * (x + 0.5) / width - 1) * 1.0;
        const py = (1 - 2 * (y + 0.5) / height) * 1.0;
        const ray = new Ray(new Vec3(0, 0, 0), new Vec3(px, py, -1).normalize());
        return scene.intersect(ray);
      });
    },
  });

  bench("render 16x16", {
    setup: () => createScene(),
    run: (scene) => {
      render(scene, 16, 16, 2);
    },
  });

  bench("render 32x32", {
    setup: () => createScene(),
    run: (scene) => {
      render(scene, 32, 32, 2);
    },
  });

  bench("render 32x32 depth 4", {
    setup: () => createScene(),
    run: (scene) => {
      render(scene, 32, 32, 4);
    },
  });
});
