{ schema, required, repeated, optional } = jsschema


schema class block_schema
  @blockId = required 'number'
  @content = required 'string'


schema class refresh_args_schema
  @groupId = required 'number'
  @groupStory = repeated block_schema
  @groupCloud = required 'object'
  @groupUsers = required 'object'


schema class refresh_schema
  @cmd = required 'string'
  @args = required refresh_args_schema


@schemas =
  block: block_schema
  refresh: refresh_args_schema
  refresh: refresh_schema
