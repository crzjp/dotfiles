local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    use 'neovim/nvim-lspconfig'
    use 'hrsh7th/nvim-cmp';
    use 'hrsh7th/cmp-nvim-lsp'
    use 'L3MON4D3/LuaSnip'

    use 'axvr/photon.vim'
end)
