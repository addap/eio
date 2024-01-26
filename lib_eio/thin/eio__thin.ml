module Promise = Promise
module Fiber = Fiber
module Cancel = Cancel
module Exn = Exn
module Private = struct
  module Suspend = Suspend
  module Broadcast = Broadcast
  module Cells = Cells
  module Ctf = Ctf
  module Debug = Debug

  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
    type _ Effect.t +=
      | Suspend = Suspend.Suspend
      | Fork = Fiber.Fork
      | Get_context = Cancel.Get_context
  end
end
