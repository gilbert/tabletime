
exports.zoneToSelector = function zoneToSelector(zone, id) {
  return zone.split('/').map(name => `.zone-${name}`).concat(id ? [`#${objectDomId(id)}`] : []).join(' ')
}

exports.escapeHtml = function escapeHtml(unsafe) {
  return unsafe
       .replace(/&/g, "&amp;")
       .replace(/</g, "&lt;")
       .replace(/>/g, "&gt;")
       .replace(/"/g, "&quot;")
       .replace(/'/g, "&#039;");
}
