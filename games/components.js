const objectComponents = new Map()

export function registerObjectComponent(type, component) {
  objectComponents.set(type, component)
}

export function getObjectComponent(type) {
  return objectComponents.get(type) || null
}
