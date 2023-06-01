//
// *   нормирование  до 0.5, 1, 1.5, 2, 2.5, 3
//
module.exports = clamp053

function clamp053(x) {
  return clamp(0.5, 3, Math.floor(x * 2) / 2)
}

function clamp(min, max, x) {
  return Math.max(min, Math.min(max, x))
}
