### Launch julia loop locally

program_mode = "local"


# run export: USA
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_export10";
include("fit_model.jl");
# main_iteration();

# run export: CHN
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_export10";
include("fit_model.jl");
# main_iteration();


# run stock export: USA
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [1, 2];
active_import_pairs = [];
core_name = "usa_stock_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [1, 2];
active_import_pairs = [];
core_name = "usa_stock_export10";
include("fit_model.jl");
# main_iteration();

# run stock export: CHN
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [1, 2];
active_import_pairs = [];
core_name = "chn_stock_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [1, 2];
active_import_pairs = [];
core_name = "chn_stock_export10";
include("fit_model.jl");
# main_iteration();


# run import: USA
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [];
stock_country = [];
active_import_pairs = [(2, 1)];
core_name = "usa_import0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [];
stock_country = [];
active_import_pairs = [(2, 1)];
core_name = "usa_import10";
include("fit_model.jl");
# main_iteration();

# run import: CHN
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [];
stock_country = [];
active_import_pairs = [(1, 2)];
core_name = "chn_import0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [];
stock_country = [];
active_import_pairs = [(1, 2)];
core_name = "chn_import10";
include("fit_model.jl");
# main_iteration();


# run reshipping: USA
ctf_mode = "reship_route";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_reship_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "reship_route";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_reship_export10";
include("fit_model.jl");
# main_iteration();

# run reshipping: CHN
ctf_mode = "reship_route_chn";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_reship_export0";
include("fit_model.jl");
# main_iteration(1_000_000, 1e-4, 1e-8, -1);

ctf_mode = "reship_route_chn";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_reship_export10";
include("fit_model.jl");
# main_iteration();


# run reshipping: USA
ctf_mode = "reship_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_reship_base_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "reship_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_reship_base_export10";
include("fit_model.jl");
# main_iteration();

# run reshipping: CHN
ctf_mode = "reship_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_reship_base_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "reship_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_reship_base_export10";
include("fit_model.jl");
# main_iteration();


# run military: USA
ctf_mode = "ally";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_ally_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_ally_export10";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_ally_base_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_ally_base_export10";
include("fit_model.jl");
# main_iteration();

# run military: CHN
ctf_mode = "ally";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_ally_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_ally_export10";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_ally_base_export0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "ally_base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_ally_base_export10";
include("fit_model.jl");
# main_iteration();


# run subsidy: USA
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(2, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_subsidy0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(2, 2)];
stock_country = [];
active_import_pairs = [];
core_name = "usa_subsidy10";
include("fit_model.jl");
# main_iteration();

# run subsidy: CHN
ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 0.01;
active_pairs = [(1, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_subsidy0";
include("fit_model.jl");
# main_iteration();

ctf_mode = "base";
mil_mode = "tax_mil";
wgt_mode = "fix";
beta_mode = "asis";
intl_wgt = 1.0;
active_pairs = [(1, 1)];
stock_country = [];
active_import_pairs = [];
core_name = "chn_subsidy10";
include("fit_model.jl");
# main_iteration();

