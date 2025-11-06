package chipyard

import org.chipsalliance.cde.config.{Config}


class aCustomConfig extends Config(
    new acustom.WithaCustomAccel ++
    new freechips.rocketchip.rocket.WithNHugeCores(1) ++
    new chipyard.config.AbstractConfig
)


class aCustomConfigTest extends Config(
    new acustom.WithaCustomTestAccel ++
    new freechips.rocketchip.rocket.WithNHugeCores(1) ++
    new chipyard.config.AbstractConfig
)