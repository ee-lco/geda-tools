['Settings', 'Environments',
 'Cloneable', 'Boundable', 'Roundable', 'Transformable',
 'MinMax', 'Geometry', 'Shape',
 'Pads', 'Layer', 'Footprint',
 'BaseFootprints', 'Footprints',
 'Renderer', 'GedaPCB', 'SVG',
].each { |file| require_relative 'IPC7351/%s' % [file] }

