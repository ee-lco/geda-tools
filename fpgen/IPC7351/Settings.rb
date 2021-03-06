require 'forwardable'

module IPC7351
    class Settings
        extend Forwardable
        def_delegators :@settings, :[], :each, :each_key, :each_pair, :each_value
        def_delegators :@settings, :has_key?, :include?, :key?, :member?
        def_delegators :@settings, :keys, :values

        def initialize(settings)
            @settings = settings
        end

        def to_h
            return @settings.clone
        end

        def to_s
            s = String.new
            @settings.keys.sort.each { |key| s += "%s=%s\n" % [key, @settings[key]] }
            return s
        end

        def select(name)
            name = "%s." % name
            return Settings.new(
                        Hash[*@settings.select { |key, value| key.start_with?(name) }.
                            map { |key, value| [key.sub(name, ""), value] }.flatten])
        end

        def merge(other)
            return strong_merge(other)
        end

        def strong_merge(other)
            case other
            when Settings
                other = other.to_h
            end

            return Settings.new(@settings.merge(other))
        end

        def weak_merge(other)
            return Settings.new(other.merge(@settings))
        end
    end
end

