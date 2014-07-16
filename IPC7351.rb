['Settings', 'Environments',
 'MinMax', 'Transformable', 'Geometry', 'Path', 'Drawable',
 'Pads', 'Layer', 'Footprint',
 'BaseFootprints', 'Footprints',
 'Renderer', 'GedaPCB', 'SVG',
].each { |file| require_relative 'IPC7351/%s' % [file] }

