return {
  {
    "vlime/vlime",
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. "/vim")
    end,
  },

  { "kovisoft/paredit" },

  {
    "hrsh7th/nvim-cmp",
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      local cmp = require("cmp")

      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            -- You could replace select_next_item() with confirm({ select = true }) to get VS Code autocompletion behavior
            cmp.select_next_item()
          elseif vim.snippet.active({ direction = 1 }) then
            vim.schedule(function()
              vim.snippet.jump(1)
            end)
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif vim.snippet.active({ direction = -1 }) then
            vim.schedule(function()
              vim.snippet.jump(-1)
            end)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
    end,
  },

  {
    "cappyzawa/trim.nvim",
    opts = {},
  },

  {
    "max397574/better-escape.nvim",
    event = "InsertCharPre",
    opts = {},
  },

  { "olimorris/onedarkpro.nvim" },

  {
    "akinsho/toggleterm.nvim",
    version = "*",
    config = true,
    keys = {
      { "<leader>t", "", desc = "Terminal" },
      { "<leader>th", "<cmd>ToggleTerm size=20 direction=horizontal<cr>", desc = "Horizontal" },
      { "<leader>tv", "<cmd>ToggleTerm size=20 directory=vertical<cr>", desc = "Vertical" },
      { "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Float" },
      { "<leader>tt", "<cmd>ToggleTerm<cr>", desc = "Toggle" },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "onedark",
    },
    keys = {
      { "<leader>fs", "<cmd>w<cr>", desc = "Save" },
    },
  },
}
