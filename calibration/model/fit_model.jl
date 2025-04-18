### Julia model simulation

using DataFrames, CSV, LinearAlgebra;
using JLD;
using LinearSolve;
using Dates;
using GLM;

# paths
CORE_PATH = joinpath("/Users", "malekseev", "Dropbox (Personal)", "research", "trade_dualuse");
OUTPUT_PATH = joinpath(CORE_PATH, "output");
STATS_PATH = joinpath(CORE_PATH, "stats");
LOGS_PATH = joinpath(CORE_PATH, "logs");

# service variables
hot_start = missing;
scale = true;
beta_mult = 10.0;
stock_mult = 0.000; # multiplier for stock depletion for countries in stock_country list
stocks_rebalance = false;
no_deficit = false;

# settings
if program_mode == "debugging"
    mil_mode_list = ["no_mil", "fix_mil", "tax_mil", "act_mil"];
    mil_mode = "act_mil";
    wgt_mode_list = ["fix", "utl"];
    wgt_mode = "fix";
    ctf_mode_list = ["base", "reship_base", "reship_route", "ally_base", "ally"];
    ctf_mode = "base"
    beta_mode_list = ["asis", "mult"];
    beta_mode = "asis";
    stock_country = [];
    intl_wgt = 0.0;
    active_pairs = [(2, 1), (1, 2)];
    active_import_pairs = [];
    core_name = "out";
end


function create_log_file()
    ts = now();
    ts = Dates.format(ts, "yyyy-mm-dd-HH-MM-SS");
    one = mil_mode;
    two = wgt_mode;
    three = ctf_mode;
    four = beta_mode;

    log_file = joinpath(LOGS_PATH, "joblog_$(one)_$(two)_$(three)_$(four)_$ts.txt");
    open(log_file, "w") do io
        println(io, "Log file created on $ts \n\n")
    end
    return log_file
end


# logging
log_file = create_log_file();


function print_log(message)
    println(message)
    open(log_file, "a") do io
        println(io, message)
    end
end


function import_data()
    print_log("Importing data for $mil_mode $wgt_mode $ctf_mode $beta_mode...")

    # final shares
    if ctf_mode == "reship_route" || ctf_mode == "reship_base"
        fd = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "fd_matrix_naics4_reshipping.csv")) |> DataFrame;
    elseif ctf_mode == "reship_route_chn" || ctf_mode == "reship_base_chn"
        fd = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "fd_matrix_naics4_reshipping_chn.csv")) |> DataFrame;
    elseif ctf_mode == "ally"
        fd = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "fd_matrix_naics4_alliances.csv")) |> DataFrame;
    elseif ctf_mode == "ally_base"
        fd = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "fd_matrix_naics4_alliances_baseline.csv")) |> DataFrame;
    else
        fd = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "fd_matrix_naics4.csv")) |> DataFrame;
    end
    sort!(fd);

    # shares
    s_C = Matrix(fd[:, ["share_consumer_chn", "share_consumer_usa", "share_consumer_row"]]);
    s_M = Matrix(fd[:, ["share_military_chn", "share_military_usa", "share_military_row"]]);

    # spending
    mil = CSV.File(joinpath(OUTPUT_PATH, "measurement", "accounting", "mil.csv")) |> DataFrame;
    gdp = CSV.File(joinpath(OUTPUT_PATH, "measurement", "accounting", "gdp.csv")) |> DataFrame;
    stock = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "stock.csv")) |> DataFrame;
    m_stock = vec(Matrix(stock)); 
    M = vec(Matrix(mil));
    if scale
        M = M ./ 1000;
        m_stock = m_stock ./ 1000;
    end
    M[3] = 0;
    wL = vec(Matrix(gdp));
    if scale
        wL = wL ./ 1000;
    end

    # prices
    prices = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "prices.csv")) |> DataFrame;
    P_C_init = [prices[1, :P_CHN], prices[1, :P_USA], prices[1, :P_ROW]];
    P_M_init = [prices[2, :P_CHN], prices[2, :P_USA], prices[2, :P_ROW]];

    # input-output matrices
    if ctf_mode == "reship_route"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_reshipping.csv")) |> DataFrame;
    elseif ctf_mode == "reship_base"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_reshipping_baseline.csv")) |> DataFrame;
    elseif ctf_mode == "reship_route_chn"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_reshipping_chn.csv")) |> DataFrame;
    elseif ctf_mode == "reship_base_chn"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_reshipping_baseline_chn.csv")) |> DataFrame;
    elseif ctf_mode == "ally"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_alliances.csv")) |> DataFrame;
    elseif ctf_mode == "ally_base"
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4_alliances_baseline.csv")) |> DataFrame;
    else
        io_matrix = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "io_matrix_naics4.csv")) |> DataFrame;
    end
    Omega = unstack(io_matrix, :industry_to, :industry_from, :share_value);
    sort!(Omega);
    @assert Omega.industry_to == fd.industry;
    Omega = Omega[:, fd.industry];
    Omega .= coalesce.(Omega, 0);
    Omega = Matrix(Omega);

    # participation matrix
    P_matrix = ones(3, 3);
    P_matrix[:, 3] .= 0;
    P_matrix[3, :] .= 0;

    # parameters
    if ctf_mode == "reship_base" || ctf_mode == "reship_route"
        df_sigma = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "elast_naics4_reshipping.csv")) |> DataFrame;
    elseif ctf_mode == "reship_base_chn" || ctf_mode == "reship_route_chn"
        df_sigma = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "elast_naics4_reshipping_chn.csv")) |> DataFrame;
    elseif ctf_mode == "ally"
        df_sigma = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "elast_naics4_alliances.csv")) |> DataFrame;
    elseif ctf_mode == "ally_base"
        df_sigma = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "elast_naics4_alliances_baseline.csv")) |> DataFrame;
    else
        df_sigma = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "elast_naics4.csv")) |> DataFrame;
    end
    sort!(df_sigma);
    @assert df_sigma.industry == fd.industry;
    sigma = df_sigma.sigma;
    gamma = 0.5;

    # firm set
    fd.country = [industry[1:3] for industry in fd.industry];
    fd.sector = [industry[5:7] for industry in fd.industry];
    ux = unique(fd.country);
    fd = transform(fd, @. :country => ByRow(isequal(ux)) .=> Symbol(:country_, lowercase.(ux)));
    firm_set = Matrix(fd[!, ["country_chn", "country_usa", "country_row"]]);

    # taxes
    if ismissing(hot_start)
        tau_start_list = Matrix{Float64}[];
        tau_import_start_list = Matrix{Float64}[];
        for j = 1:3
            push!(tau_start_list, ones(length(sigma), 3))
            push!(tau_import_start_list, ones(length(sigma), 3))
        end
    else
        tau_frame = CSV.File(joinpath(STATS_PATH, "calibration", hot_start * ".csv")) |> DataFrame;
        tau_start_list = Matrix{Float64}[];
        tau_import_start_list = Matrix{Float64}[];
        cty_list = ["CHN", "USA", "ROW"];
        for j = 1:3
            tau_start_matrix = ones(nrow(tau_frame), 3);
            tau_import_start_matrix = ones(nrow(tau_frame), 3);
            for k = 1:2
                if k != j
                    col = tau_frame[!, "tau_X_" * cty_list[j] * "_" * cty_list[k]];
                    col[ismissing.(col)] .= 1.0;
                    tau_start_matrix[:, k] = copy(col);

                    col = tau_frame[!, "tau_M_" * cty_list[j] * "_" * cty_list[k]];
                    col[ismissing.(col)] .= 1.0;
                    tau_import_start_matrix[:, k] = copy(col);
                end
            end
            push!(tau_start_list, copy(tau_start_matrix));
            push!(tau_import_start_list, copy(tau_import_start_matrix));
        end
    end
    
    # industry attributes
    if ctf_mode == "reship_base" || ctf_mode == "reship_route"
        attrib = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "attrib_naics4_reshipping.csv")) |> DataFrame;
    elseif ctf_mode == "reship_base_chn" || ctf_mode == "reship_route_chn"
        attrib = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "attrib_naics4_reshipping_chn.csv")) |> DataFrame;
    elseif ctf_mode == "ally"
        attrib = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "attrib_naics4_alliances.csv")) |> DataFrame;
    elseif ctf_mode == "ally_base"
        attrib = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "attrib_naics4_alliances_baseline.csv")) |> DataFrame;
    else
        attrib = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "attrib_naics4.csv")) |> DataFrame;
    end
    sort!(attrib)
    setdiff(fd.industry, attrib.industry)
    @assert attrib.industry == fd.industry;
    dualuse = attrib.dualuse_outcome;
    return s_M, s_C, P_M_init, P_C_init, Omega, M, m_stock, wL, sigma, gamma, firm_set, tau_start_list, tau_import_start_list, attrib, P_matrix, dualuse
end


function initialize_data()
    s_M, s_C, P_M_init, P_C_init, Omega, M, m_stock, wL, sigma, gamma, firm_set, tau_start_list, tau_import_start_list, attrib, P_matrix, dualuse = import_data();

    # normalize
    s_M = s_M ./ sum(s_M, dims = 1);
    s_C = s_C ./ sum(s_C, dims = 1);
    row_sum = sum(Omega, dims = 2);
    row_sum[row_sum .<= 1.0] .= 1.0;
    Omega = Omega ./ row_sum;

    # select empty categories 
    print_log("Filtering network nodes...")
    Psi_tilde = inv(I - Omega);
    Psi_tilde[Psi_tilde .< 0.0] .= 0.0;
    sales = Psi_tilde' * (s_C * (wL - M) + s_M * M);
    nonempty_set = findall(sales .> 1e-8);
    print_log("Kept $(length(nonempty_set)) out of $(length(sales))");
    s_M = s_M[nonempty_set, :];
    s_C = s_C[nonempty_set, :];
    Omega = Omega[nonempty_set, nonempty_set];
    OmegaLOne = 1 .- sum(Omega, dims = 2);
    OmegaLOne[OmegaLOne .<= 1e-12] .= 0;
    sigma = sigma[nonempty_set];
    firm_set = firm_set[nonempty_set, :];
    attrib = attrib[nonempty_set, :];
    OmegaL = OmegaLOne .* firm_set;
    OmegaL = max.(OmegaL, 0);
    OmegaL_tilde = copy(OmegaL);

    # clear deficit
    Psi_tilde = inv(I - Omega);
    Psi_tilde[Psi_tilde .< 0.0] .= 0.0;
    Psi = copy(Psi_tilde);
    
    A = OmegaL_tilde' * Psi_tilde' * s_C;
    b = wL - OmegaL_tilde' * Psi_tilde' * (s_C * (wL - M) + s_M * M);
    prob = LinearProblem(A, b, u0 = zeros(length(wL)));
    sol = solve(prob);
    D = sol.u;
    if no_deficit
        D = zeros(3);
        A = I - OmegaL_tilde' * Psi_tilde' * s_C;
        b = OmegaL_tilde' * Psi_tilde' * (s_C * (D - M) + s_M * M);
        A_small = A[[1, 3], [1, 3]];
        b_small = b[[1, 3]] - [A[1, 2] * wL[2], A[3, 2] * wL[2]];
        wL_small = inv(A_small) * b_small;
        wL[1] = wL_small[1];
        wL[3] = wL_small[2];
    end
    
    # @assert maximum(abs.((wL .- OmegaL_tilde' * Psi_tilde' * (s_C * (wL + D - M) + s_M * M)) ./ wL)) < 1e-9;
    C = wL + D - M;
    C_real = C ./ P_C_init;
    M_real = M ./ P_M_init;
    X = Psi_tilde' * (s_C * C + s_M * M);
    R = zeros(3);
    Rdest = zeros(3, 3);
    OmegaXP = zeros(size(Omega)[1], 3);
    for j in 1:3
        OmegaXP[:, j] = sum(firm_set[:, j]' .* Omega, dims = 2);
    end
    F = X .* OmegaXP;
    FL = X .* OmegaL;
    MP = zeros(3, 3);
    for i in 1:3
        for j in 1:3
            MP[i, j] = F[:, i]' * firm_set[:, j];
        end
    end

    # utility weights
    U_C = ones(3, 3);
    if wgt_mode == "fix"
        for i in 1:3
            U_C[i, :] .= P_C_init[i]
        end
        for i in 1:3
            for j in 1:3
                if i != j
                    U_C[i, j] = intl_wgt * P_C_init[i];
                end
            end
        end
    end
    if wgt_mode == "utl"
        for i in 1:3
            U_C[i, :] .= P_C_init[i]
        end
    end

    U_M_start = zeros(3, 3);
    beta = zeros(3);
    nu = zeros(3, 3);
    if mil_mode != "no_mil"
        for ix in 1:2
            P_vector = P_matrix[:, ix];
            nu_vect = P_vector .* (M ./ P_M_init) .^ gamma;
            nu_vect = nu_vect ./ sum(nu_vect);
            nu[:, ix] = nu_vect;
            beta[ix] = M[ix] / (gamma * nu_vect[ix] * (1 - nu_vect[ix]));
            for ix_j in 1:3
                if ix_j == ix
                    U_M_start[ix_j, ix] = 1.0;
                else
                    U_M_start[ix_j, ix] = -gamma * beta[ix] / M[ix] * nu_vect[ix] * nu_vect[ix_j];
                end
            end
        end
    end
    U_cons_start = U_C' * C_real;
    U_mil_start = Diagonal(nu) * beta;
    U_int_start = Diagonal(U_C) * C_real + U_mil_start;
    U_start = U_cons_start + U_mil_start;

    # hat algebra
    hat_p = ones(size(Omega)[1]);
    hat_w = ones(3);

    # Lambda
    LambdaR = zeros(3, size(Omega)[1]);
    LambdaX = Matrix{Float64}(I, length(X), length(X));
    LambdaL = I - OmegaL_tilde' * Psi_tilde' * s_C;

    # sets for hat algebra updating
    vals = sort(unique(sigma), rev = true);
    dict_sigma = [];
    dict_sigma_rest = [];
    for val in vals
        nodes_sigma = findall(sigma .== val);
        if length(nodes_sigma) < 10
            dict_sigma_rest = vcat(dict_sigma_rest, nodes_sigma);
        end
        dict_sigma = vcat(dict_sigma, (val, nodes_sigma));
    end

    # indices for importers and export taxes
    pair_ix = Dict();
    tau_X = Dict();
    tau_X_init = Dict();
    tau_X_start = Dict();
    tau_M = Dict();
    tau_M_init = Dict();
    tau_M_start = Dict();
    for i in 1:3
        dict_pair_ix = Dict();
        dict_tau_X = Dict();
        dict_tau_X_init = Dict();
        dict_tau_X_start = Dict();
        dict_tau_M = Dict();
        dict_tau_M_init = Dict();
        dict_tau_M_start = Dict();
        for j in 1:3
            if i != j
                dict_pair_ix[j] = findall(F[:, j] .* firm_set[:, i] .> 0.0);
                dict_tau_X[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_X_init[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_X_start[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_M[j] = tau_import_start_list[i][dict_pair_ix[j], j];
                dict_tau_M_init[j] = tau_import_start_list[i][dict_pair_ix[j], j];
                dict_tau_M_start[j] = tau_import_start_list[i][dict_pair_ix[j], j];
            end
            if i == j
                dict_pair_ix[j] = findall(OmegaL[:, j] .> 0.0);
                dict_tau_X[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_X_init[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_X_start[j] = tau_start_list[i][dict_pair_ix[j], j];
                dict_tau_M[j] = tau_import_start_list[i][dict_pair_ix[j], j];
                dict_tau_M_init[j] = tau_import_start_list[i][dict_pair_ix[j], j];
                dict_tau_M_start[j] = tau_import_start_list[i][dict_pair_ix[j], j];
            end
        end
        pair_ix[i] = dict_pair_ix;
        tau_X[i] = dict_tau_X;
        tau_X_init[i] = dict_tau_X_init;
        tau_X_start[i] = dict_tau_X_start;
        tau_M[i] = dict_tau_M;
        tau_M_init[i] = dict_tau_M_init;
        tau_M_start[i] = dict_tau_M_start;
    end

    # change in dM
    dM_R_init = zeros(3, 3);
    dM_X_init = zeros(size(Omega)[1], 3);
    dM_logw_init = zeros(3, 3);
    dM_logp_init = zeros(size(Omega)[1], 3);
    dM_logP_C_init = zeros(3, 3);
    dM_logP_M_init = zeros(3, 3);
    dM_R = zeros(3, 3);
    dM_X = zeros(size(Omega)[1], 3);
    dM_logw = zeros(3, 3);
    dM_logp = zeros(size(Omega)[1], 3);
    dM_logP_C = zeros(3, 3);
    dM_logP_M = zeros(3, 3);

    # change in dT
    dT_logw_init = Dict();
    dT_logp_init = Dict();
    dT_logP_C_init = Dict();
    dT_logP_M_init = Dict();
    dT_logP_C_tax_init = Dict();
    dT_logP_M_tax_init = Dict();
    dT_logP_C_wage_init = Dict();
    dT_logP_M_wage_init = Dict();
    dT_X_init = Dict();
    dT_R_init = Dict();
    dT_Rdest_init = Dict();
    dT_logF_init = Dict();
    dT_logw = Dict();
    dT_logp = Dict();
    dT_logP_C = Dict();
    dT_logP_M = Dict();
    dT_logP_C_tax = Dict();
    dT_logP_M_tax = Dict();
    dT_logP_C_wage = Dict();
    dT_logP_M_wage = Dict();
    dT_X = Dict();
    dT_R = Dict();
    dT_Rdest = Dict();
    dT_logF = Dict();
    for i in 1:3
        dict_dT_logw_init = Dict();
        dict_dT_logp_init = Dict();
        dict_dT_logP_C_init = Dict();
        dict_dT_logP_M_init = Dict();
        dict_dT_logP_C_tax_init = Dict();
        dict_dT_logP_M_tax_init = Dict();
        dict_dT_logP_C_wage_init = Dict();
        dict_dT_logP_M_wage_init = Dict();
        dict_dT_X_init = Dict();
        dict_dT_Rdest_init = Dict();
        dict_dT_R_init = Dict();
        dict_dT_logF_init = Dict();
        dict_dT_logw = Dict();
        dict_dT_logp = Dict();
        dict_dT_logP_C = Dict();
        dict_dT_logP_M = Dict();
        dict_dT_logP_C_tax = Dict();
        dict_dT_logP_M_tax = Dict();
        dict_dT_logP_C_wage = Dict();
        dict_dT_logP_M_wage = Dict();
        dict_dT_X = Dict();
        dict_dT_Rdest = Dict();
        dict_dT_R = Dict();
        dict_dT_logF = Dict();
        for j in 1:3
            dict_dT_logw_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logw[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logp_init[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dT_logp[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dT_logP_C_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_C[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_C_tax_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_C_tax[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M_tax_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M_tax[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_C_wage_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_C_wage[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M_wage_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logP_M_wage[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_X_init[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dT_X[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dT_Rdest_init[j] = zeros(3, 3, length(pair_ix[i][j]));
            dict_dT_Rdest[j] = zeros(3, 3, length(pair_ix[i][j]));
            dict_dT_R_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_R[j] = zeros(3, length(pair_ix[i][j]));
            dict_dT_logF_init[j] = zeros(length(pair_ix[i][j]), length(pair_ix[i][j]));
            dict_dT_logF[j] = zeros(length(pair_ix[i][j]), length(pair_ix[i][j]));
        end
        dT_logw_init[i] = dict_dT_logw_init;
        dT_logp_init[i] = dict_dT_logp_init;
        dT_logP_C_init[i] = dict_dT_logP_C_init;
        dT_logP_M_init[i] = dict_dT_logP_M_init;
        dT_logP_C_tax_init[i] = dict_dT_logP_C_tax_init;
        dT_logP_M_tax_init[i] = dict_dT_logP_M_tax_init;
        dT_logP_C_wage_init[i] = dict_dT_logP_C_wage_init;
        dT_logP_M_wage_init[i] = dict_dT_logP_M_wage_init;
        dT_X_init[i] = dict_dT_X_init;
        dT_Rdest_init[i] = dict_dT_Rdest_init;
        dT_R_init[i] = dict_dT_R_init;
        dT_logF_init[i] = dict_dT_logF_init;
        dT_logw[i] = dict_dT_logw;
        dT_logp[i] = dict_dT_logp;
        dT_logP_C[i] = dict_dT_logP_C;
        dT_logP_M[i] = dict_dT_logP_M;
        dT_logP_C_tax[i] = dict_dT_logP_C_tax;
        dT_logP_M_tax[i] = dict_dT_logP_M_tax;
        dT_logP_C_wage[i] = dict_dT_logP_C_wage;
        dT_logP_M_wage[i] = dict_dT_logP_M_wage;
        dT_X[i] = dict_dT_X;
        dT_Rdest[i] = dict_dT_Rdest;
        dT_R[i] = dict_dT_R;
        dT_logF[i] = dict_dT_logF;
    end

    # change in dTM
    dTM_logw_init = Dict();
    dTM_logp_init = Dict();
    dTM_logP_C_init = Dict();
    dTM_logP_M_init = Dict();
    dTM_logP_C_tax_init = Dict();
    dTM_logP_M_tax_init = Dict();
    dTM_logP_C_wage_init = Dict();
    dTM_logP_M_wage_init = Dict();
    dTM_X_init = Dict();
    dTM_R_init = Dict();
    dTM_Rdest_init = Dict();
    dTM_logF_init = Dict();
    dTM_logw = Dict();
    dTM_logp = Dict();
    dTM_logP_C = Dict();
    dTM_logP_M = Dict();
    dTM_logP_C_tax = Dict();
    dTM_logP_M_tax = Dict();
    dTM_logP_C_wage = Dict();
    dTM_logP_M_wage = Dict();
    dTM_X = Dict();
    dTM_R = Dict();
    dTM_Rdest = Dict();
    dTM_logF = Dict();
    for i in 1:3
        dict_dTM_logw_init = Dict();
        dict_dTM_logp_init = Dict();
        dict_dTM_logP_C_init = Dict();
        dict_dTM_logP_M_init = Dict();
        dict_dTM_logP_C_tax_init = Dict();
        dict_dTM_logP_M_tax_init = Dict();
        dict_dTM_logP_C_wage_init = Dict();
        dict_dTM_logP_M_wage_init = Dict();
        dict_dTM_X_init = Dict();
        dict_dTM_Rdest_init = Dict();
        dict_dTM_R_init = Dict();
        dict_dTM_logF_init = Dict();
        dict_dTM_logw = Dict();
        dict_dTM_logp = Dict();
        dict_dTM_logP_C = Dict();
        dict_dTM_logP_M = Dict();
        dict_dTM_logP_C_tax = Dict();
        dict_dTM_logP_M_tax = Dict();
        dict_dTM_logP_C_wage = Dict();
        dict_dTM_logP_M_wage = Dict();
        dict_dTM_X = Dict();
        dict_dTM_Rdest = Dict();
        dict_dTM_R = Dict();
        dict_dTM_logF = Dict();
        for j in 1:3
            dict_dTM_logw_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logw[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logp_init[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dTM_logp[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dTM_logP_C_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_C[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_C_tax_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_C_tax[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M_tax_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M_tax[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_C_wage_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_C_wage[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M_wage_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logP_M_wage[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_X_init[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dTM_X[j] = zeros(size(Omega)[1], length(pair_ix[i][j]));
            dict_dTM_Rdest_init[j] = zeros(3, 3, length(pair_ix[i][j]));
            dict_dTM_Rdest[j] = zeros(3, 3, length(pair_ix[i][j]));
            dict_dTM_R_init[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_R[j] = zeros(3, length(pair_ix[i][j]));
            dict_dTM_logF_init[j] = zeros(length(pair_ix[i][j]), length(pair_ix[i][j]));
            dict_dTM_logF[j] = zeros(length(pair_ix[i][j]), length(pair_ix[i][j]));
        end
        dTM_logw_init[i] = dict_dTM_logw_init;
        dTM_logp_init[i] = dict_dTM_logp_init;
        dTM_logP_C_init[i] = dict_dTM_logP_C_init;
        dTM_logP_M_init[i] = dict_dTM_logP_M_init;
        dTM_logP_C_tax_init[i] = dict_dTM_logP_C_tax_init;
        dTM_logP_M_tax_init[i] = dict_dTM_logP_M_tax_init;
        dTM_logP_C_wage_init[i] = dict_dTM_logP_C_wage_init;
        dTM_logP_M_wage_init[i] = dict_dTM_logP_M_wage_init;
        dTM_X_init[i] = dict_dTM_X_init;
        dTM_Rdest_init[i] = dict_dTM_Rdest_init;
        dTM_R_init[i] = dict_dTM_R_init;
        dTM_logF_init[i] = dict_dTM_logF_init;
        dTM_logw[i] = dict_dTM_logw;
        dTM_logp[i] = dict_dTM_logp;
        dTM_logP_C[i] = dict_dTM_logP_C;
        dTM_logP_M[i] = dict_dTM_logP_M;
        dTM_logP_C_tax[i] = dict_dTM_logP_C_tax;
        dTM_logP_M_tax[i] = dict_dTM_logP_M_tax;
        dTM_logP_C_wage[i] = dict_dTM_logP_C_wage;
        dTM_logP_M_wage[i] = dict_dTM_logP_M_wage;
        dTM_X[i] = dict_dTM_X;
        dTM_Rdest[i] = dict_dTM_Rdest;
        dTM_R[i] = dict_dTM_R;
        dTM_logF[i] = dict_dTM_logF;
    end

    # change in the model
    cent_M_init = (s_M' * Psi)';
    cent_C_init = (s_C' * Psi)';
    adj_cent_M_init = cent_M_init ./ sum(cent_M_init, dims = 1) * 100;
    adj_cent_C_init = cent_C_init ./ sum(cent_C_init, dims = 1) * 100;
    
    E = s_M .* M' + s_C .* C';
    E_M = s_M .* M';
    E_C = s_C .* C';
    E_rob = copy(E);
    E_rob[E_rob .<= 0] .= 1;
    S_M = s_M ./ E_rob .* M';
    S_C = s_C ./ E_rob .* C';
    check_S = S_M + S_C;
    
    C_D = Psi' * E ./ X;
    C_M = Psi' * (s_M .* M') ./ X;
    C_C = Psi' * (s_C .* C') ./ X;
    C_D_sigma = C_D ./ sigma;
    C_M_sigma = C_M ./ sigma;
    C_C_sigma = C_C ./ sigma;

    S_L = X .* OmegaL ./ wL';

    # tariffs
    T_init = Dict();
    T_rev_init = Dict();
    T_wage_init = Dict();
    T_rev_row_init = Dict();
    T_price_C_PE_init = Dict();
    T_price_M_PE_init = Dict();
    T_price_C_GE_init = Dict();
    T_price_M_GE_init = Dict();
    T_income_init = Dict();
    T_price_C_init = Dict();
    T_price_M_init = Dict();
    T_price_PE_init = Dict();
    T_price_GE_init = Dict();
    T = Dict();
    T_rev = Dict();
    T_wage = Dict();
    T_rev_row = Dict();
    T_price_C_PE = Dict();
    T_price_M_PE = Dict();
    T_price_C_GE = Dict();
    T_price_M_GE = Dict();
    T_income = Dict();
    T_price_C = Dict();
    T_price_M = Dict();
    T_price_PE = Dict();
    T_price_GE = Dict();
    wgt_T = Dict();
    delta_T = Dict();
    for i in 1:3
        dict_T_init = Dict();
        dict_T_rev_init = Dict();
        dict_T_wage_init = Dict();
        dict_T_rev_row_init = Dict();
        dict_T_price_C_PE_init = Dict();
        dict_T_price_M_PE_init = Dict();
        dict_T_price_C_GE_init = Dict();
        dict_T_price_M_GE_init = Dict();
        dict_T_income_init = Dict();
        dict_T_price_C_init = Dict();
        dict_T_price_M_init = Dict();
        dict_T_price_PE_init = Dict();
        dict_T_price_GE_init = Dict();
        dict_T = Dict();
        dict_T_rev = Dict();
        dict_T_wage = Dict();
        dict_T_rev_row = Dict();
        dict_T_price_C_PE = Dict();
        dict_T_price_M_PE = Dict();
        dict_T_price_C_GE = Dict();
        dict_T_price_M_GE = Dict();
        dict_T_income = Dict();
        dict_T_price_C = Dict();
        dict_T_price_M = Dict();
        dict_T_price_PE = Dict();
        dict_T_price_GE = Dict();
        dict_wgt_T = Dict();
        dict_delta_T = Dict();
        for j in 1:3
            dict_T_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_rev_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_wage_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_rev_row_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_income_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_T[j] = zeros(length(pair_ix[i][j]));
            dict_T_rev[j] = zeros(length(pair_ix[i][j]));
            dict_T_wage[j] = zeros(length(pair_ix[i][j]));
            dict_T_rev_row[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C_PE[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M_PE[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C_GE[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M_GE[j] = zeros(length(pair_ix[i][j]));
            dict_T_income[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_C[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_M[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_PE[j] = zeros(length(pair_ix[i][j]));
            dict_T_price_GE[j] = zeros(length(pair_ix[i][j]));
            dict_wgt_T[j] = ones(length(pair_ix[i][j]));
            dict_delta_T[j] = ones(length(pair_ix[i][j]));
        end
        T_init[i] = dict_T_init;
        T_rev_init[i] = dict_T_rev_init;
        T_wage_init[i] = dict_T_wage_init;
        T_rev_row_init[i] = dict_T_rev_row_init;
        T_price_C_PE_init[i] = dict_T_price_C_PE_init;
        T_price_M_PE_init[i] = dict_T_price_M_PE_init;
        T_price_C_GE_init[i] = dict_T_price_C_GE_init;
        T_price_M_GE_init[i] = dict_T_price_M_GE_init;
        T_income_init[i] = dict_T_income_init;
        T_price_C_init[i] = dict_T_price_C_init;
        T_price_M_init[i] = dict_T_price_M_init;
        T_price_PE_init[i] = dict_T_price_PE_init;
        T_price_GE_init[i] = dict_T_price_GE_init;
        T[i] = dict_T;
        T_rev[i] = dict_T_rev;
        T_wage[i] = dict_T_wage;
        T_rev_row[i] = dict_T_rev_row;
        T_price_C_PE[i] = dict_T_price_C_PE;
        T_price_M_PE[i] = dict_T_price_M_PE;
        T_price_C_GE[i] = dict_T_price_C_GE;
        T_price_M_GE[i] = dict_T_price_M_GE;
        T_income[i] = dict_T_income;
        T_price_C[i] = dict_T_price_C;
        T_price_M[i] = dict_T_price_M;
        T_price_PE[i] = dict_T_price_PE;
        T_price_GE[i] = dict_T_price_GE;
        wgt_T[i] = dict_wgt_T;
        delta_T[i] = dict_delta_T;
    end

    # change in TM
    TM_init = Dict();
    TM_rev_init = Dict();
    TM_wage_init = Dict();
    TM_rev_row_init = Dict();
    TM_price_C_PE_init = Dict();
    TM_price_M_PE_init = Dict();
    TM_price_C_GE_init = Dict();
    TM_price_M_GE_init = Dict();
    TM_income_init = Dict();
    TM_price_C_init = Dict();
    TM_price_M_init = Dict();
    TM_price_PE_init = Dict();
    TM_price_GE_init = Dict();
    TM = Dict();
    TM_rev = Dict();
    TM_wage = Dict();
    TM_rev_row = Dict();
    TM_price_C_PE = Dict();
    TM_price_M_PE = Dict();
    TM_price_C_GE = Dict();
    TM_price_M_GE = Dict();
    TM_income = Dict();
    TM_price_C = Dict();
    TM_price_M = Dict();
    TM_price_PE = Dict();
    TM_price_GE = Dict();
    wgt_TM = Dict();
    delta_TM = Dict();
    for i in 1:3
        dict_TM_init = Dict();
        dict_TM_rev_init = Dict();
        dict_TM_wage_init = Dict();
        dict_TM_rev_row_init = Dict();
        dict_TM_price_C_PE_init = Dict();
        dict_TM_price_M_PE_init = Dict();
        dict_TM_price_C_GE_init = Dict();
        dict_TM_price_M_GE_init = Dict();
        dict_TM_income_init = Dict();
        dict_TM_price_C_init = Dict();
        dict_TM_price_M_init = Dict();
        dict_TM_price_PE_init = Dict();
        dict_TM_price_GE_init = Dict();
        dict_TM = Dict();
        dict_TM_rev = Dict();
        dict_TM_wage = Dict();
        dict_TM_rev_row = Dict();
        dict_TM_price_C_PE = Dict();
        dict_TM_price_M_PE = Dict();
        dict_TM_price_C_GE = Dict();
        dict_TM_price_M_GE = Dict();
        dict_TM_income = Dict();
        dict_TM_price_C = Dict();
        dict_TM_price_M = Dict();
        dict_TM_price_PE = Dict();
        dict_TM_price_GE = Dict();
        dict_wgt_TM = Dict();
        dict_delta_TM = Dict();
        for j in 1:3
            dict_TM_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_rev_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_wage_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_rev_row_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_income_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_PE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_GE_init[j] = zeros(length(pair_ix[i][j]));
            dict_TM[j] = zeros(length(pair_ix[i][j]));
            dict_TM_rev[j] = zeros(length(pair_ix[i][j]));
            dict_TM_wage[j] = zeros(length(pair_ix[i][j]));
            dict_TM_rev_row[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C_PE[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M_PE[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C_GE[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M_GE[j] = zeros(length(pair_ix[i][j]));
            dict_TM_income[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_C[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_M[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_PE[j] = zeros(length(pair_ix[i][j]));
            dict_TM_price_GE[j] = zeros(length(pair_ix[i][j]));
            dict_wgt_TM[j] = ones(length(pair_ix[i][j]));
            dict_delta_TM[j] = ones(length(pair_ix[i][j]));
        end
        TM_init[i] = dict_TM_init;
        TM_rev_init[i] = dict_TM_rev_init;
        TM_wage_init[i] = dict_TM_wage_init;
        TM_rev_row_init[i] = dict_TM_rev_row_init;
        TM_price_C_PE_init[i] = dict_TM_price_C_PE_init;
        TM_price_M_PE_init[i] = dict_TM_price_M_PE_init;
        TM_price_C_GE_init[i] = dict_TM_price_C_GE_init;
        TM_price_M_GE_init[i] = dict_TM_price_M_GE_init;
        TM_income_init[i] = dict_TM_income_init;
        TM_price_C_init[i] = dict_TM_price_C_init;
        TM_price_M_init[i] = dict_TM_price_M_init;
        TM_price_PE_init[i] = dict_TM_price_PE_init;
        TM_price_GE_init[i] = dict_TM_price_GE_init;
        TM[i] = dict_TM;
        TM_rev[i] = dict_TM_rev;
        TM_wage[i] = dict_TM_wage;
        TM_rev_row[i] = dict_TM_rev_row;
        TM_price_C_PE[i] = dict_TM_price_C_PE;
        TM_price_M_PE[i] = dict_TM_price_M_PE;
        TM_price_C_GE[i] = dict_TM_price_C_GE;
        TM_price_M_GE[i] = dict_TM_price_M_GE;
        TM_income[i] = dict_TM_income;
        TM_price_C[i] = dict_TM_price_C;
        TM_price_M[i] = dict_TM_price_M;
        TM_price_PE[i] = dict_TM_price_PE;
        TM_price_GE[i] = dict_TM_price_GE;
        wgt_TM[i] = dict_wgt_TM;
        delta_TM[i] = dict_delta_TM;
    end
    tau_avg = ones(3);

    # save dict
    model = Dict(
        "firm_set" => firm_set,
        "pair_ix" => pair_ix,
        "dict_sigma" => dict_sigma,
        "dict_sigma_rest" => dict_sigma_rest,
        "sigma" => sigma,
        "gamma" => gamma,
        "s_M" => s_M,
        "s_C" => s_C,
        "P_matrix" => P_matrix,
        "P_M_init" => P_M_init,
        "P_C_init" => P_C_init,
        "Omega_init" => Omega,
        "OmegaL_init" => OmegaL,
        "OmegaL_tilde" => copy(OmegaL_tilde),
        "wL_init" => wL,
        "D_init" => copy(D),
        "D" => D,
        "m_stock" => copy(m_stock),
        "M_init" => copy(M),
        "C_init" => copy(C),
        "wL" => wL,
        "X_init" => copy(X),
        "F_init" => copy(F),
        "F" => copy(F),
        "FL_init" => copy(FL),
        "FL" => copy(FL),
        "X" => X,
        "F" => F,
        "MP_init" => copy(MP),
        "MP" => copy(MP),
        "M" => copy(M),
        "C" => copy(C),
        "R" => R,
        "Rdest" => Rdest,
        "P_M" => P_M_init,
        "P_C" => P_C_init,
        "C_real_init" => copy(C_real),
        "C_real" => C_real,
        "M_real_init" => copy(M_real),
        "M_real" => M_real,
        "Omega" => Omega,
        "Omega_tilde" => Omega,
        "OmegaL" => OmegaL,
        "OmegaL_tilde" => copy(OmegaL_tilde),
        "OmegaXP" => OmegaXP,
        "OmegaXP_init" => copy(OmegaXP),
        "Psi" => Psi,
        "Psi_tilde" => Psi,     
        "hat_p_init" => copy(hat_p),
        "hat_p" => hat_p,
        "hat_w_init" => copy(hat_w),
        "hat_w" => hat_w,
        "attrib" => attrib,
        "LambdaR" => LambdaR,
        "LambdaX" => LambdaX,
        "LambdaL" => LambdaL,
        "tau_X_start" => tau_X_start,
        "tau_M_start" => tau_M_start,
        "tau_avg_init" => copy(tau_avg),
        "tau_avg" => copy(tau_avg),
        "dM_logw_init" => dM_logw_init,
        "dM_logp_init" => dM_logp_init,
        "dM_logP_C_init" => dM_logP_C_init,
        "dM_logP_M_init" => dM_logP_M_init,
        "dM_X_init" => dM_X_init,
        "dM_R_init" => dM_R_init,
        "dM_logw" => dM_logw,
        "dM_logp" => dM_logp,
        "dM_logP_C" => dM_logP_C,
        "dM_logP_M" => dM_logP_M,
        "dM_X" => dM_X,
        "dM_R" => dM_R,
        "dT_logw_init" => dT_logw_init,
        "dT_logp_init" => dT_logp_init,
        "dT_logP_C_init" => dT_logP_C_init,
        "dT_logP_M_init" => dT_logP_M_init,
        "dT_logP_C_wage_init" => dT_logP_C_wage_init,
        "dT_logP_M_wage_init" => dT_logP_M_wage_init,
        "dT_logP_C_tax_init" => dT_logP_C_tax_init,
        "dT_logP_M_tax_init" => dT_logP_M_tax_init,
        "dT_logF_init" => dT_logF_init,
        "dT_Rdest_init" => dT_Rdest_init,
        "dT_X_init" => dT_X_init,
        "dT_R_init" => dT_R_init,
        "dT_logw" => dT_logw,
        "dT_logp" => dT_logp,
        "dT_logP_C" => dT_logP_C,
        "dT_logP_M" => dT_logP_M,
        "dT_logP_C_tax" => dT_logP_C_tax,
        "dT_logP_M_tax" => dT_logP_M_tax,
        "dT_logP_C_wage" => dT_logP_C_wage,
        "dT_logP_M_wage" => dT_logP_M_wage,
        "dT_logF" => dT_logF,
        "dT_Rdest" => dT_Rdest,
        "dT_X" => dT_X,
        "dT_R" => dT_R,
        "dTM_logw_init" => dTM_logw_init,
        "dTM_logp_init" => dTM_logp_init,
        "dTM_logP_C_init" => dTM_logP_C_init,
        "dTM_logP_M_init" => dTM_logP_M_init,
        "dTM_logP_C_wage_init" => dTM_logP_C_wage_init,
        "dTM_logP_M_wage_init" => dTM_logP_M_wage_init,
        "dTM_logP_C_tax_init" => dTM_logP_C_tax_init,
        "dTM_logP_M_tax_init" => dTM_logP_M_tax_init,
        "dTM_logF_init" => dTM_logF_init,
        "dTM_Rdest_init" => dTM_Rdest_init,
        "dTM_X_init" => dTM_X_init,
        "dTM_R_init" => dTM_R_init,
        "dTM_logw" => dTM_logw,
        "dTM_logp" => dTM_logp,
        "dTM_logP_C" => dTM_logP_C,
        "dTM_logP_M" => dTM_logP_M,
        "dTM_logP_C_tax" => dTM_logP_C_tax,
        "dTM_logP_M_tax" => dTM_logP_M_tax,
        "dTM_logP_C_wage" => dTM_logP_C_wage,
        "dTM_logP_M_wage" => dTM_logP_M_wage,
        "dTM_logF" => dTM_logF,
        "dTM_Rdest" => dTM_Rdest,
        "dTM_X" => dTM_X,
        "dTM_R" => dTM_R,
        "U_start" => copy(U_start),
        "U_init" => copy(U_start),
        "U" => copy(U_start),
        "U_int_start" => copy(U_int_start),
        "U_cons_start" => copy(U_cons_start),
        "U_mil_start" => copy(U_mil_start),
        "U_int_init" => copy(U_int_start),
        "U_cons_init" => copy(U_cons_start),
        "U_mil_init" => copy(U_mil_start),
        "U_int" => copy(U_int_start),
        "U_cons" => copy(U_cons_start),
        "U_mil" => copy(U_mil_start),
        "beta_start" => copy(beta),
        "beta_init" => copy(beta),
        "beta" => beta,
        "nu_init" => copy(nu),
        "nu" => nu,
        "U_C" => U_C,
        "U_M_start" => copy(U_M_start),
        "U_M_init" => copy(U_M_start),
        "U_M" => copy(U_M_start),
        "cent_M_init" => copy(cent_M_init),
        "cent_C_init" => copy(cent_C_init),
        "adj_cent_M_init" => copy(adj_cent_M_init),
        "adj_cent_C_init" => copy(adj_cent_C_init),
        "E_init" => copy(E),
        "E_M_init" => copy(E_M),
        "E_C_init" => copy(E_C),
        "S_M_init" => copy(S_M),
        "S_C_init" => copy(S_C),
        "C_D_init" => copy(C_D),
        "C_M_init" => copy(C_M),
        "C_C_init" => copy(C_C),
        "C_D_sigma_init" => copy(C_D_sigma),
        "C_M_sigma_init" => copy(C_M_sigma),
        "C_C_sigma_init" => copy(C_C_sigma),
        "S_L_init" => copy(S_L),
        "cent_M" => copy(cent_M_init),
        "cent_C" => copy(cent_C_init),
        "adj_cent_M" => copy(adj_cent_M_init),
        "adj_cent_C" => copy(adj_cent_C_init),
        "E" => copy(E),
        "E_M" => copy(E_M),
        "E_C" => copy(E_C),
        "S_M" => copy(S_M),
        "S_C" => copy(S_C),
        "C_D" => copy(C_D),
        "C_M" => copy(C_M),
        "C_C" => copy(C_C),
        "C_D_sigma" => copy(C_D_sigma),
        "C_M_sigma" => copy(C_M_sigma),
        "C_C_sigma" => copy(C_C_sigma),
        "S_L" => copy(S_L),
        "T_init" => T_init,
        "T_rev_init" => T_rev_init,
        "T_wage_init" => T_wage_init,
        "T_rev_row_init" => T_rev_row_init,
        "T_price_C_PE_init" => T_price_C_PE_init,
        "T_price_M_PE_init" => T_price_M_PE_init,
        "T_price_C_GE_init" => T_price_C_GE_init,
        "T_price_M_GE_init" => T_price_M_GE_init,
        "T_income_init" => T_income_init,
        "T_price_C_init" => T_price_C_init,
        "T_price_M_init" => T_price_M_init,
        "T_price_PE_init" => T_price_PE_init,
        "T_price_GE_init" => T_price_GE_init,
        "tau_X_init" => tau_X_init,
        "tau_M_init" => tau_M_init,
        "T" => T,
        "T_rev" => T_rev,
        "T_wage" => T_wage,
        "T_rev_row" => T_rev_row,
        "T_price_C_PE" => T_price_C_PE,
        "T_price_M_PE" => T_price_M_PE,
        "T_price_C_GE" => T_price_C_GE,
        "T_price_M_GE" => T_price_M_GE,
        "T_income" => T_income,
        "T_price_C" => T_price_C,
        "T_price_M" => T_price_M,
        "T_price_PE" => T_price_PE,
        "T_price_GE" => T_price_GE,
        "wgt_T" => wgt_T,
        "delta_T" => delta_T,
        "T_init" => T_init,
        "T_rev_init" => T_rev_init,
        "T_wage_init" => T_wage_init,
        "T_rev_row_init" => T_rev_row_init,
        "T_price_C_PE_init" => T_price_C_PE_init,
        "T_price_M_PE_init" => T_price_M_PE_init,
        "T_price_C_GE_init" => T_price_C_GE_init,
        "T_price_M_GE_init" => T_price_M_GE_init,
        "T_income_init" => T_income_init,
        "T_price_C_init" => T_price_C_init,
        "T_price_M_init" => T_price_M_init,
        "T_price_PE_init" => T_price_PE_init,
        "T_price_GE_init" => T_price_GE_init,
        "tau_X_init" => tau_X_init,
        "tau_M_init" => tau_M_init,
        "TM" => TM,
        "TM_rev" => TM_rev,
        "TM_wage" => TM_wage,
        "TM_rev_row" => TM_rev_row,
        "TM_price_C_PE" => TM_price_C_PE,
        "TM_price_M_PE" => TM_price_M_PE,
        "TM_price_C_GE" => TM_price_C_GE,
        "TM_price_M_GE" => TM_price_M_GE,
        "TM_income" => TM_income,
        "TM_price_C" => TM_price_C,
        "TM_price_M" => TM_price_M,
        "TM_price_PE" => TM_price_PE,
        "TM_price_GE" => TM_price_GE,
        "wgt_TM" => wgt_TM,
        "delta_TM" => delta_TM,
        "TM_init" => TM_init,
        "TM_rev_init" => TM_rev_init,
        "TM_wage_init" => TM_wage_init,
        "TM_rev_row_init" => TM_rev_row_init,
        "TM_price_C_PE_init" => TM_price_C_PE_init,
        "TM_price_M_PE_init" => TM_price_M_PE_init,
        "TM_price_C_GE_init" => TM_price_C_GE_init,
        "TM_price_M_GE_init" => TM_price_M_GE_init,
        "TM_income_init" => TM_income_init,
        "TM_price_C_init" => TM_price_C_init,
        "TM_price_M_init" => TM_price_M_init,
        "TM_price_PE_init" => TM_price_PE_init,
        "TM_price_GE_init" => TM_price_GE_init,
        "tau_X" => tau_X,
        "tau_M" => tau_M,
        "dualuse" => dualuse
    );
    return model
end


function get_dlogw_dP(model, initial = false)
    print_log("Processing policy jacobian...")

    # unchanged parameters
    tau_X = model["tau_X"];
    tau_M = model["tau_M"];
    Omega = model["Omega"];
    OmegaL = model["OmegaL"];
    OmegaXP = model["OmegaXP"];
    Omega_tilde = model["Omega_tilde"];
    OmegaL_tilde = model["OmegaL_tilde"];
    Psi = model["Psi"];
    Psi_tilde = model["Psi_tilde"];
    X = model["X"];
    wL = model["wL"];
    D = model["D"];
    M = copy(model["M"]);
    M_init = copy(model["M_init"]);
    wL_init = model["wL_init"];
    D_init = model["D_init"];
    F = model["F"];
    FL = model["FL"];
    LambdaR = model["LambdaR"];
    LambdaX = model["LambdaX"];
    LambdaL = model["LambdaL"];
    pair_ix = model["pair_ix"];
    firm_set = model["firm_set"];
    s_C = copy(model["s_C"]);
    s_M = copy(model["s_M"]);
    sigma = model["sigma"];

    if mil_mode == "tax_mil"
        M = zeros(3);
        s_C = s_C .* (1 .- M_init ./ (wL_init + D_init))' .+ s_M .* (M_init ./ (wL_init + D_init))';
    end

    # core types
    # print_log("Processing policy jacobian: calculate core...")
    core1 = Psi_tilde' * (s_C * (D - M) + s_M * M);
    core2 = LambdaX * core1;
    core3 = Psi_tilde' * s_C * LambdaR * core2;
    core4 = Psi_tilde' * s_C * wL;
    core5 = LambdaX * core4;
    core6 = Psi_tilde' * s_C * LambdaR * core5;

    # block types
    # print_log("Processing policy jacobian: calculate blocks...")
    block0 = LambdaX * Psi_tilde';
    block1 = OmegaL_tilde' * block0;
    block2 = Psi * OmegaL;
    
    # get operator function
    function get_wage_operator(core, matrix)
        matNumer = Diagonal(vec(core' * matrix));
        matDenom = zeros(size(matrix)[2], length(wL));
        matDenom_eye = zeros(size(matrix)[2], size(Omega)[1]);
        I_eye = Diagonal(ones(size(Omega)[1])); 
        for ix_j in 1:(size(matrix)[2])
            matDenom[ix_j, :] .= vec((core .* matrix[:, ix_j])' * block2);
            matDenom_eye[ix_j, :] .= vec((core .* matrix[:, ix_j])' * I_eye);
        end
        return matNumer, matDenom, matDenom_eye;
    end

    function get_revenue_wage_operator(core)
        operator_dLambdaR = zeros(length(wL), length(wL));
        operator_eye_dLambdaR = zeros(length(wL), size(Omega)[1]);
        operator_dLambdaR_import = zeros(length(wL), length(wL));   
        operator_eye_dLambdaR_import = zeros(length(wL), size(Omega)[1]);
        operator_dLambdaR_export = zeros(length(wL), length(wL));   
        operator_eye_dLambdaR_export = zeros(length(wL), size(Omega)[1]);
        for j in 1:3
            for k in 1:2
                if k == j
                    Omega_tax = zeros(size(OmegaL)[1], 1);
                    ix_k = findall(OmegaL[:, k] .> 0);
                    Omega_tax[pair_ix[j][k], 1] = (tau_X[j][k] .- 1) ./ tau_X[j][k] .* OmegaL[pair_ix[j][k], k];

                    matNumer_dLambdaR, matDenom_dLambdaR, matDenom_eye_dLambdaR = get_wage_operator(core, Omega_tax);
                    operator_dLambdaR[k, :] = operator_dLambdaR[k, :] .+ matNumer_dLambdaR[1] .- vec(matDenom_dLambdaR);
                    operator_eye_dLambdaR[k, :] = operator_eye_dLambdaR[k, :] .+ matNumer_dLambdaR[1] .- vec(matDenom_eye_dLambdaR);
                    operator_dLambdaR_export[k, :] = operator_dLambdaR_export[k, :] .+ matNumer_dLambdaR[1] .- vec(matDenom_dLambdaR);
                    operator_eye_dLambdaR_export[k, :] = operator_eye_dLambdaR_export[k, :] .+ matNumer_dLambdaR[1] .- vec(matDenom_eye_dLambdaR);
                end
                if k != j
                    Omega_tax = zeros(size(Omega));
                    ix_k = findall(firm_set[:, k] .== 1);
                    Omega_tax[pair_ix[j][k], ix_k] = (tau_X[j][k] .- 1) ./ tau_X[j][k] .* Omega[pair_ix[j][k], ix_k] ./ tau_M[j][k];
                    matNumer_dLambdaR, matDenom_dLambdaR, matDenom_eye_dLambdaR = get_wage_operator(core, Omega_tax);
                    operator_dLambdaR[k, :] = operator_dLambdaR[k, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR * block2 - matDenom_dLambdaR));
                    operator_eye_dLambdaR[k, :] = operator_eye_dLambdaR[k, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR - matDenom_eye_dLambdaR));
                    operator_dLambdaR_export[k, :] = operator_dLambdaR_export[k, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR * block2 - matDenom_dLambdaR));
                    operator_eye_dLambdaR_export[k, :] = operator_eye_dLambdaR_export[k, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR - matDenom_eye_dLambdaR));

                    Omega_tax = zeros(size(Omega));
                    ix_k = findall(firm_set[:, k] .== 1);
                    Omega_tax[pair_ix[j][k], ix_k] = (tau_M[j][k] .- 1) ./ tau_M[j][k] .* Omega[pair_ix[j][k], ix_k];
                    matNumer_dLambdaR, matDenom_dLambdaR, matDenom_eye_dLambdaR = get_wage_operator(core, Omega_tax);
                    operator_dLambdaR[j, :] = operator_dLambdaR[j, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR * block2 - matDenom_dLambdaR));
                    operator_eye_dLambdaR[j, :] = operator_eye_dLambdaR[j, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR - matDenom_eye_dLambdaR));
                    operator_dLambdaR_import[j, :] = operator_dLambdaR_import[j, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR * block2 - matDenom_dLambdaR));
                    operator_eye_dLambdaR_import[j, :] = operator_eye_dLambdaR_import[j, :] + vec(ones(size(Omega)[1])' * (matNumer_dLambdaR - matDenom_eye_dLambdaR));
                end
            end
        end
        return operator_dLambdaR, operator_eye_dLambdaR, operator_dLambdaR_import, operator_eye_dLambdaR_import, operator_dLambdaR_export, operator_eye_dLambdaR_export;
    end

    # rhs: dOmegaL term
    # print_log("Processing policy jacobian: dOmegaL...")
    core = (1 .- sigma) .* core2;
    matNumer_dOmegaL, matDenom_dOmegaL, matDenom_eye_dOmegaL = get_wage_operator(core, OmegaL_tilde);
    operator_dOmegaL = matNumer_dOmegaL - matDenom_dOmegaL;
    operator_eye_dOmegaL = -matDenom_eye_dOmegaL;

    # rhs: dPsi_tilde term
    # print_log("Processing policy jacobian: dPsi_tilde...")
    core = (1 .- sigma) .* core1;
    matNumer_dPsi_tilde, matDenom_dPsi_tilde, matDenom_eye_dPsi_tilde = get_wage_operator(core, Omega_tilde);
    operator_dPsi_tilde = block1 * (matNumer_dPsi_tilde * block2 - matDenom_dPsi_tilde);
    operator_eye_dPsi_tilde = block1 * (matNumer_dPsi_tilde - matDenom_eye_dPsi_tilde);

    # rhs: LambdaX first term
    # print_log("Processing policy jacobian: dLambdaX first...")
    core = (1 .- sigma) .* core3;
    matNumer_dLambdaX_first, matDenom_dLambdaX_first, matDenom_eye_dLambdaX_first = get_wage_operator(core, Omega_tilde);
    operator_dLambdaX_first = block1 * (matNumer_dLambdaX_first * block2 - matDenom_dLambdaX_first);
    operator_eye_dLambdaX_first = block1 * (matNumer_dLambdaX_first - matDenom_eye_dLambdaX_first);

    # rhs: LambdaX second term
    # print_log("Processing policy jacobian: dLambdaX second...")
    operator_dLambdaR = zeros(length(wL), length(wL));
    operator_eye_dLambdaR = zeros(length(wL), size(Omega)[1]);
    core = (1 .- sigma) .* core2;
    operator_dLambdaR, operator_eye_dLambdaR, _, _, _, _ = get_revenue_wage_operator(core);
    operator_dLambdaX_second = block1 * s_C * operator_dLambdaR;
    operator_eye_dLambdaX_second = block1 * s_C * operator_eye_dLambdaR;

    # lhs: dlogw direct term
    # print_log("Processing policy jacobian: dlogw...")
    operator_dlogw = LambdaL * Diagonal(wL);

    # lhs: dLambdaL first term
    # print_log("Processing policy jacobian: dLambdaL first...")
    core = (1 .- sigma) .* core5;
    matNumer_dLambdaL_first, matDenom_dLambdaL_first, matDenom_eye_dLambdaL_first = get_wage_operator(core, OmegaL_tilde);
    operator_dLambdaL_first = -matNumer_dLambdaL_first + matDenom_dLambdaL_first;
    operator_eye_dLambdaL_first = matDenom_eye_dLambdaL_first;

    # lhs: dLambdaL second term
    # print_log("Processing policy jacobian: dLambdaL second...")
    core = (1 .- sigma) .* core4;
    matNumer_dLambdaL_second, matDenom_dLambdaL_second, matDenom_eye_dLambdaL_second = get_wage_operator(core, Omega_tilde);
    operator_dLambdaL_second = -block1 * (matNumer_dLambdaL_second * block2 - matDenom_dLambdaL_second);
    operator_eye_dLambdaL_second = -block1 * (matNumer_dLambdaL_second - matDenom_eye_dLambdaL_second);

    # lhs: dLambdaL third term
    # print_log("Processing policy jacobian: dLambdaL third...")
    core = (1 .- sigma) .* core6;
    matNumer_dLambdaL_third, matDenom_dLambdaL_third, matDenom_eye_dLambdaL_third = get_wage_operator(core, Omega_tilde);
    operator_dLambdaL_third = -block1 * (matNumer_dLambdaL_third * block2 - matDenom_dLambdaL_third);
    operator_eye_dLambdaL_third = -block1 * (matNumer_dLambdaL_third - matDenom_eye_dLambdaL_third);

    # lhs: dLambdaL fourth term
    # print_log("Processing policy jacobian: dLambdaL fourth...")
    operator_dLambdaR = zeros(length(wL), length(wL));
    operator_eye_dLambdaR = zeros(length(wL), size(Omega)[1]);
    core = (1 .- sigma) .* core5;
    operator_dLambdaR, operator_eye_dLambdaR, _, _, _, _ = get_revenue_wage_operator(core);
    operator_dLambdaL_fourth = -block1 * s_C * operator_dLambdaR;
    operator_eye_dLambdaL_fourth = -block1 * s_C * operator_eye_dLambdaR;

    # dX operator: first
    # print_log("Processing policy jacobian: dX first...")
    core = (1 .- sigma) .* (core1 + core4);
    matNumer_dX_first, matDenom_dX_first, matDenom_eye_dX_first = get_wage_operator(core, Omega_tilde);
    operator_dX_first = block0 * (matNumer_dX_first * block2 - matDenom_dX_first);
    operator_eye_dX_first = block0 * (matNumer_dX_first- matDenom_eye_dX_first);

    # dX operator: second
    # print_log("Processing policy jacobian: dX second...")
    core = (1 .- sigma) .* (core3 + core6);
    matNumer_dX_second, matDenom_dX_second, matDenom_eye_dX_second = get_wage_operator(core, Omega_tilde);
    operator_dX_second = block0 * (matNumer_dX_second * block2 - matDenom_dX_second);
    operator_eye_dX_second = block0 * (matNumer_dX_second - matDenom_eye_dX_second);

    # dX operator: third
    # print_log("Processing policy jacobian: dX third...")
    operator_dLambdaR = zeros(length(wL), length(wL));
    operator_eye_dLambdaR = zeros(length(wL), size(Omega)[1]);
    core = (1 .- sigma) .* (core2 + core5);
    operator_dLambdaR, operator_eye_dLambdaR, _, _, _, _ = get_revenue_wage_operator(core);
    operator_dX_third = block0 * s_C * operator_dLambdaR;
    operator_eye_dX_third = block0 * s_C * operator_eye_dLambdaR;

    # dR operator: first
    # print_log("Processing policy jacobian: dR first...")
    operator_dLambdaR = zeros(length(wL), length(wL));
    operator_eye_dLambdaR = zeros(length(wL), size(Omega)[1]);
    core = (1 .- sigma) .* X;
    operator_dLambdaR, operator_eye_dLambdaR, operator_dLambdaR_import, operator_eye_dLambdaR_import, operator_dLambdaR_export, operator_eye_dLambdaR_export = get_revenue_wage_operator(core);
    operator_dR = operator_dLambdaR;
    operator_eye_dR = operator_eye_dLambdaR;

    # formulate the system
    A = (
        operator_dlogw
        + operator_dLambdaL_first + operator_dLambdaL_second + operator_dLambdaL_third + operator_dLambdaL_fourth
        - operator_dOmegaL
        - operator_dPsi_tilde
        - operator_dLambdaX_first - operator_dLambdaX_second
    );
    A_small = A[1:2, 1:2];
    A_small_inv = inv(A_small);

    B = -(
        operator_eye_dLambdaL_first + operator_eye_dLambdaL_second + operator_eye_dLambdaL_third + operator_eye_dLambdaL_fourth
        - operator_eye_dOmegaL
        - operator_eye_dPsi_tilde
        - operator_eye_dLambdaX_first - operator_eye_dLambdaX_second
    );

    # set tax jacobian
    ix_imp = 2;
    ix_exp = 2;
    for (ix_imp, ix_exp) in active_pairs
        # print_log("Processing policy jacobian: $ix_imp, $ix_exp...")

        if ix_exp != ix_imp
            dT_logT = Psi[:, pair_ix[ix_imp][ix_exp]] * Diagonal(OmegaXP[pair_ix[ix_imp][ix_exp], ix_exp]);
        else
            dT_logT = Psi[:, pair_ix[ix_imp][ix_exp]] * Diagonal(OmegaL[pair_ix[ix_imp][ix_exp], ix_exp]);
        end
            
        vect_dT = B * dT_logT;

        if ix_exp != ix_imp
            dLambdaR_const = (1 ./ tau_X[ix_imp][ix_exp]) .* OmegaXP[pair_ix[ix_imp][ix_exp], ix_exp] ./ tau_M[ix_imp][ix_exp];
        else
            dLambdaR_const = (1 ./ tau_X[ix_imp][ix_exp]) .* OmegaL[pair_ix[ix_imp][ix_exp], ix_exp];
        end
        rhs_LambdaX = block1 * s_C[:, ix_exp] .* (dLambdaR_const .* (core2[pair_ix[ix_imp][ix_exp]] + core5[pair_ix[ix_imp][ix_exp]]))';

        if ix_exp != ix_imp
            firms_exp = findall(firm_set[:, ix_exp]);
            dOmega_tilde_tax = -sigma[pair_ix[ix_imp][ix_exp]] .* Omega_tilde[pair_ix[ix_imp][ix_exp], firms_exp];
            rhs_tax = block1[:, firms_exp] * (dOmega_tilde_tax .* (
                core1[pair_ix[ix_imp][ix_exp]] + core3[pair_ix[ix_imp][ix_exp]]
                + core4[pair_ix[ix_imp][ix_exp]] + core6[pair_ix[ix_imp][ix_exp]]
            ))';
        else
            firms_exp = [ix_exp];
            dOmegaL_tax = (-sigma[pair_ix[ix_imp][ix_exp]] .* OmegaL_tilde[pair_ix[ix_imp][ix_exp], :])';
            rhs_tax = dOmegaL_tax .* (block0 * (core1 + core3 + core4 + core6))[pair_ix[ix_imp][ix_exp]]';
        end

        b = vect_dT + rhs_LambdaX + rhs_tax;
        b_small = b[1:2, :];
        sol_small = A_small_inv * b_small;
        dT_logw = zeros(3, length(pair_ix[ix_imp][ix_exp]));
        dT_logw[1:2, :] = sol_small;

        dT_logp = dT_logT + Psi * OmegaL * dT_logw;
        dT_logP_C = s_C' * dT_logp;
        dT_logP_M = s_M' * dT_logp;
        dT_logP_C_tax = s_C' * dT_logT;
        dT_logP_M_tax = s_M' * dT_logT;
        dT_logP_C_wage = s_C' * Psi * OmegaL * dT_logw;
        dT_logP_M_wage = s_M' * Psi * OmegaL * dT_logw;

        if ix_exp != ix_imp
            dT_X = (
                (operator_dX_first + operator_dX_second + operator_dX_third) * dT_logw
                + (operator_eye_dX_first + operator_eye_dX_second + operator_eye_dX_third) * dT_logT
                + block0 * (
                    s_C[:, ix_exp] * (dLambdaR_const .* (core2[pair_ix[ix_imp][ix_exp]] + core5[pair_ix[ix_imp][ix_exp]]))'
                    + s_C * Diagonal(wL) * dT_logw
                ) + block0[:, firms_exp] * (dOmega_tilde_tax .* (
                    core1[pair_ix[ix_imp][ix_exp]] + core3[pair_ix[ix_imp][ix_exp]]
                    + core4[pair_ix[ix_imp][ix_exp]] + core6[pair_ix[ix_imp][ix_exp]]
                ))'
            );
        else
            dT_X = (
                (operator_dX_first + operator_dX_second + operator_dX_third) * dT_logw
                + (operator_eye_dX_first + operator_eye_dX_second + operator_eye_dX_third) * dT_logT
                + block0 * (
                    s_C[:, ix_exp] * (dLambdaR_const .* (core2[pair_ix[ix_imp][ix_exp]] + core5[pair_ix[ix_imp][ix_exp]]))'
                    + s_C * Diagonal(wL) * dT_logw
                )
            );
        end

        operator_dR_export = zeros(size(operator_dLambdaR_export));
        operator_eye_dR_export = zeros(size(operator_eye_dLambdaR_export));
        operator_dR_export[ix_exp, :] = operator_dLambdaR_export[ix_exp, :];
        operator_eye_dR_export[ix_exp, :] = operator_eye_dLambdaR_export[ix_exp, :];
        LambdaR_zeros = zeros(size(LambdaR));
        LambdaR_zeros[ix_exp, :] = LambdaR[ix_exp, :];
        dT_R = (
            operator_dR * dT_logw
            + operator_eye_dR * dT_logT
            + LambdaR * dT_X
        ) - (
            operator_dLambdaR_export * dT_logw
            + operator_eye_dLambdaR_export * dT_logT
            + LambdaR_zeros * dT_X
        );
            
        I_eye = Diagonal(ones(size(Omega)[1]))[:, pair_ix[ix_imp][ix_exp]];
        if ix_exp != ix_imp
            F_rob = copy(F[pair_ix[ix_imp][ix_exp], ix_exp]);
            F_rob[F_rob .<= 0] .= 1;    
            price_effect = (1 .- sigma) .* X .* Omega .* firm_set[:, ix_exp]' * dT_logp;
            index_effect = OmegaXP[:, ix_exp] .* (1 .- sigma) .* X .* dT_logp;
            tax_effect = (OmegaXP[:, ix_exp] .* (1 .- sigma) .* X) .* I_eye;
            dT_F = price_effect - index_effect + tax_effect;
            dT_logF = dT_F[pair_ix[ix_imp][ix_exp], :] ./ F_rob - I;
        else
            F_rob = copy(FL[pair_ix[ix_imp][ix_exp], ix_exp]);
            F_rob[F_rob .<= 0] .= 1;   
            price_effect = (1 .- sigma) .* X .* OmegaL[:, ix_imp] .* dT_logw[ix_imp, :]';
            index_effect = OmegaL[:, ix_imp] .* (1 .- sigma) .* X .* dT_logp;
            tax_effect = (OmegaL[:, ix_imp] .* (1 .- sigma) .* X) .* I_eye;
            dT_F = price_effect - index_effect + tax_effect;
            dT_logF = dT_F[pair_ix[ix_imp][ix_exp], :] ./ F_rob - I;
        end

        model["dT_logw"][ix_imp][ix_exp] .= copy(dT_logw);
        model["dT_logp"][ix_imp][ix_exp] .= copy(dT_logp);
        model["dT_logP_C"][ix_imp][ix_exp] .= copy(dT_logP_C);
        model["dT_logP_M"][ix_imp][ix_exp] .= copy(dT_logP_M);
        model["dT_logP_C_tax"][ix_imp][ix_exp] .= copy(dT_logP_C_tax);
        model["dT_logP_M_tax"][ix_imp][ix_exp] .= copy(dT_logP_M_tax);
        model["dT_logP_C_wage"][ix_imp][ix_exp] .= copy(dT_logP_C_wage);
        model["dT_logP_M_wage"][ix_imp][ix_exp] .= copy(dT_logP_M_wage);
        model["dT_X"][ix_imp][ix_exp] .= copy(dT_X);
        model["dT_R"][ix_imp][ix_exp] .= copy(dT_R);
        model["dT_logF"][ix_imp][ix_exp] .= copy(dT_logF);

        if initial
            model["dT_logw_init"][ix_imp][ix_exp] .= copy(dT_logw);
            model["dT_logp_init"][ix_imp][ix_exp] .= copy(dT_logp);
            model["dT_logP_C_init"][ix_imp][ix_exp] .= copy(dT_logP_C);
            model["dT_logP_M_init"][ix_imp][ix_exp] .= copy(dT_logP_M);
            model["dT_logP_C_tax_init"][ix_imp][ix_exp] .= copy(dT_logP_C_tax);
            model["dT_logP_M_tax_init"][ix_imp][ix_exp] .= copy(dT_logP_M_tax);
            model["dT_logP_C_wage_init"][ix_imp][ix_exp] .= copy(dT_logP_C_wage);
            model["dT_logP_M_wage_init"][ix_imp][ix_exp] .= copy(dT_logP_M_wage);
            model["dT_X_init"][ix_imp][ix_exp] .= copy(dT_X);
            model["dT_R_init"][ix_imp][ix_exp] .= copy(dT_R);
            model["dT_logF_init"][ix_imp][ix_exp] .= copy(dT_logF);
        end
    end

    ix_imp = 2;
    ix_exp = 1;
    for (ix_imp, ix_exp) in active_import_pairs
        # print_log("Processing policy jacobian: $ix_imp, $ix_exp...")

        if ix_exp != ix_imp
            dT_logT = Psi[:, pair_ix[ix_imp][ix_exp]] * Diagonal(OmegaXP[pair_ix[ix_imp][ix_exp], ix_exp]);
        else
            println("error");
        end
            
        vect_dT = B * dT_logT;

        if ix_exp != ix_imp
            dLambdaR_const = (1 ./ tau_M[ix_imp][ix_exp]) .* OmegaXP[pair_ix[ix_imp][ix_exp], ix_exp];
        else
            println("error");
        end
        rhs_LambdaX = block1 * s_C[:, ix_imp] * (dLambdaR_const .* (core2[pair_ix[ix_imp][ix_exp]] + core5[pair_ix[ix_imp][ix_exp]]))';
                
        if ix_exp != ix_imp
            firms_exp = findall(firm_set[:, ix_exp]);
            dOmega_tilde_tax = -sigma[pair_ix[ix_imp][ix_exp]] .* Omega_tilde[pair_ix[ix_imp][ix_exp], firms_exp] .* tau_X[ix_imp][ix_exp];
        else
            println("error");
        end
        rhs_tax = block1[:, firms_exp] * (dOmega_tilde_tax .* (
            core1[pair_ix[ix_imp][ix_exp]] + core3[pair_ix[ix_imp][ix_exp]]
            + core4[pair_ix[ix_imp][ix_exp]] + core6[pair_ix[ix_imp][ix_exp]]
        ))';
            
        b = vect_dT + rhs_LambdaX + rhs_tax;
        b_small = b[1:2, :];
        sol_small = A_small_inv * b_small;
        dT_logw = zeros(3, length(pair_ix[ix_imp][ix_exp]));
        dT_logw[1:2, :] = sol_small;

        dT_logp = dT_logT + Psi * OmegaL * dT_logw;
        dT_logP_C = s_C' * dT_logp;
        dT_logP_M = s_M' * dT_logp;
        dT_logP_C_tax = s_C' * dT_logT;
        dT_logP_M_tax = s_M' * dT_logT;
        dT_logP_C_wage = s_C' * Psi * OmegaL * dT_logw;
        dT_logP_M_wage = s_M' * Psi * OmegaL * dT_logw;

        dT_X = (
            (operator_dX_first + operator_dX_second + operator_dX_third) * dT_logw
            + (operator_eye_dX_first + operator_eye_dX_second + operator_eye_dX_third) * dT_logT
            + block0 * (
                s_C[:, ix_exp] * (dLambdaR_const .* (core2[pair_ix[ix_imp][ix_exp]] + core5[pair_ix[ix_imp][ix_exp]]))'
                + s_C * Diagonal(wL) * dT_logw
            ) + block0[:, firms_exp] * (dOmega_tilde_tax .* (
                core1[pair_ix[ix_imp][ix_exp]] + core3[pair_ix[ix_imp][ix_exp]]
                + core4[pair_ix[ix_imp][ix_exp]] + core6[pair_ix[ix_imp][ix_exp]]
            ))'
        );

        operator_dR_import = zeros(size(operator_dLambdaR_import));
        operator_eye_dR_import = zeros(size(operator_eye_dLambdaR_import));
        operator_dR_import[ix_imp, :] = operator_dLambdaR_import[ix_imp, :];
        operator_eye_dR_import[ix_imp, :] = operator_eye_dLambdaR_import[ix_imp, :];
        LambdaR_zeros = zeros(size(LambdaR));
        LambdaR_zeros[ix_imp, :] = LambdaR[ix_imp, :];
        dT_R = (
            operator_dR * dT_logw
            + operator_eye_dR * dT_logT
            + LambdaR * dT_X
        ) - (
            operator_dLambdaR_import * dT_logw
            + operator_eye_dLambdaR_import * dT_logT
            + LambdaR_zeros * dT_X
        );
            
        I_eye = Diagonal(ones(size(Omega)[1]))[:, pair_ix[ix_imp][ix_exp]];
        if ix_exp != ix_imp
            F_rob = copy(F[pair_ix[ix_imp][ix_exp], ix_exp]);
            F_rob[F_rob .<= 0] .= 1;    
            price_effect = (1 .- sigma) .* X .* Omega .* firm_set[:, ix_exp]' * dT_logp;
            index_effect = OmegaXP[:, ix_exp] .* (1 .- sigma) .* X .* dT_logp;
            tax_effect = (OmegaXP[:, ix_exp] .* (1 .- sigma) .* X) .* I_eye;
            dT_F = price_effect - index_effect + tax_effect;
            dT_logF = dT_F[pair_ix[ix_imp][ix_exp], :] ./ F_rob - I;
        else
            println("error");
        end

        model["dTM_logw"][ix_imp][ix_exp] .= copy(dT_logw);
        model["dTM_logp"][ix_imp][ix_exp] .= copy(dT_logp);
        model["dTM_logP_C"][ix_imp][ix_exp] .= copy(dT_logP_C);
        model["dTM_logP_M"][ix_imp][ix_exp] .= copy(dT_logP_M);
        model["dTM_logP_C_tax"][ix_imp][ix_exp] .= copy(dT_logP_C_tax);
        model["dTM_logP_M_tax"][ix_imp][ix_exp] .= copy(dT_logP_M_tax);
        model["dTM_logP_C_wage"][ix_imp][ix_exp] .= copy(dT_logP_C_wage);
        model["dTM_logP_M_wage"][ix_imp][ix_exp] .= copy(dT_logP_M_wage);
        model["dTM_X"][ix_imp][ix_exp] .= copy(dT_X);
        model["dTM_R"][ix_imp][ix_exp] .= copy(dT_R);
        model["dTM_logF"][ix_imp][ix_exp] .= copy(dT_logF);

        if initial
            model["dTM_logw_init"][ix_imp][ix_exp] .= copy(dT_logw);
            model["dTM_logp_init"][ix_imp][ix_exp] .= copy(dT_logp);
            model["dTM_logP_C_init"][ix_imp][ix_exp] .= copy(dT_logP_C);
            model["dTM_logP_M_init"][ix_imp][ix_exp] .= copy(dT_logP_M);
            model["dTM_logP_C_tax_init"][ix_imp][ix_exp] .= copy(dT_logP_C_tax);
            model["dTM_logP_M_tax_init"][ix_imp][ix_exp] .= copy(dT_logP_M_tax);
            model["dTM_logP_C_wage_init"][ix_imp][ix_exp] .= copy(dT_logP_C_wage);
            model["dTM_logP_M_wage_init"][ix_imp][ix_exp] .= copy(dT_logP_M_wage);
            model["dTM_X_init"][ix_imp][ix_exp] .= copy(dT_X);
            model["dTM_R_init"][ix_imp][ix_exp] .= copy(dT_R);
            model["dTM_logF_init"][ix_imp][ix_exp] .= copy(dT_logF);
        end
    end

    if mil_mode == "tax_mode"
        M = copy(model["M"]);
        s_M = copy(model["s_M"]);
        s_C = copy(model["s_C"]);
    end
    if mil_mode != "no_mil"
        # set military jacobian
        # print_log("Processing policy jacobian: military...")

        b = block1 * (-s_C + s_M);
        sol_small = A_small_inv * b[1:2, :];
        dM_logw = zeros(3, 3);
        dM_logw[1:2, :] .= copy(sol_small);

        dM_logp = block2 * dM_logw;
        dM_logP_C = s_C' * dM_logp;
        dM_logP_M = s_M' * dM_logp;
        dM_X = (
            (operator_dX_first + operator_dX_second + operator_dX_third) * dM_logw
            + block0 * s_C * Diagonal(wL) * dM_logw
            + block0 * (-s_C + s_M)
        );
        dM_R = operator_dR * dM_logw + LambdaR * dM_X;

        model["dM_logw"] = copy(dM_logw);
        model["dM_logp"] = copy(dM_logp);
        model["dM_logP_C"] = copy(dM_logP_C);
        model["dM_logP_M"] = copy(dM_logP_M);
        model["dM_X"] = copy(dM_X);
        model["dM_R"] = copy(dM_R);
        if initial
            model["dM_logw_init"] = copy(dM_logw);
            model["dM_logp_init"] = copy(dM_logp);
            model["dM_logP_C_init"] = copy(dM_logP_C);
            model["dM_logP_M_init"] = copy(dM_logP_M);
            model["dM_X_init"] = copy(dM_X);
            model["dM_R_init"] = copy(dM_R);
        end
    end
end


function output_beta()
    model = initialize_data();
    m_stock_alt = CSV.File(joinpath(OUTPUT_PATH, "calibration", "iotables", "stock_variants.csv")) |> DataFrame;
    m_stock_alt[!, :ROW] .= 0;
    m_stock_mat = Matrix(m_stock_alt[!, 2:4]) ./ 1000;

    M_init = copy(model["M_init"]);
    C_init = copy(model["C_init"]);
    C_real = model["C_real_init"];
    P_M_init = model["P_M_init"];
    P_C_init = model["P_C_init"];
    P_matrix = model["P_matrix"];
    gamma = model["gamma"];
    U_C = model["U_C"];
    U_C[2, 1] = 1;
    U_C[3, 1] = 1;
    U_C[1, 2] = U_C[1, 1];
    U_C[3, 2] = 1;
    wL_init = model["wL_init"];
    m_output = (repeat(M_init ./ P_M_init, outer = (1, 4))' .+ m_stock_mat) ./ model["wL_init"][2]';

    mil_mode = "act_mil";
    get_dlogw_dP(model);
    dM_logP_C_fix = copy(model["dM_logP_C"]);
    dM_logP_M_fix = copy(model["dM_logP_M"]);

    share_C = (model["OmegaL"]' * model["Psi"]' * model["s_C"]) .* 100;
    share_M = (model["OmegaL"]' * model["Psi"]' * model["s_M"]) .* 100;
    df_res = DataFrame(
        vcat([share_C[:, 1], share_M[:, 1], share_C[:, 2], share_M[:, 2], share_C[:, 3], share_M[:, 3]]),
        ["share_CHN_C", "share_CHN_M", "share_USA_C", "share_USA_M", "share_ROW_C", "share_ROW_M"]
    );
    CSV.write(joinpath(STATS_PATH, "calibration", "model", "beta", "shares.csv"), df_res);

    # conflict share
    change_C = model["dM_logP_C"] .* 100;
    change_M = model["dM_logP_M"] .* 100;
    change_w = model["dM_logw"] .* 100;
    df_res = DataFrame(
        vcat([
            change_w[:, 1], change_C[:, 1], change_M[:, 1],
            change_w[:, 2], change_C[:, 2], change_M[:, 2],
            change_w[:, 3], change_C[:, 3], change_M[:, 3]
        ]),
        ["change_CHN_w", "change_CHN_C", "change_CHN_M", "change_USA_w", "change_USA_C", "change_USA_M",
            "change_ROW_w", "change_ROW_C", "change_ROW_M"]
    );
    CSV.write(joinpath(STATS_PATH, "calibration", "model", "beta", "changes.csv"), df_res);

    beta_matrix = zeros(3, 4 * 2);
    beta_ratio_matrix = zeros(3, 4 * 2);
    M_matrix = zeros(3, 4 * 2);
    for i = 1:4
        m_stock = m_stock_mat[i, :];
        nu = zeros(3, 3);
        for ix in 1:2
            P_vector = P_matrix[:, ix];
            nu_vect = P_vector .* (m_stock + M_init ./ P_M_init) .^ gamma;
            nu_vect = nu_vect ./ sum(nu_vect);
            nu[:, ix] .= nu_vect;
        end

        # partial equilibrium
        for ix in 1:2
            wgt_C = U_C[:, ix] ./ P_C_init;
    
            wgt_M = -gamma ./ (m_stock .+ M_init ./ P_M_init) .* nu[ix, ix] .* nu[:, ix] ./ P_M_init;
            wgt_M[3] = 0.0;
            wgt_M[ix] = gamma / (m_stock[ix] .+ M_init[ix] ./ P_M_init[ix]) * nu[ix, ix] * (1 - nu[ix, ix]) ./ P_M_init[ix];
    
            C_part_pe = wgt_C[ix];
            M_part_pe = wgt_M[ix];
            beta_matrix[1, (i - 1) * 2 + ix] = C_part_pe / M_part_pe;
            beta_ratio_matrix[1, (i - 1) * 2 + ix] = C_part_pe / M_part_pe / wL_init[2] / U_C[ix, ix];

            C_part_fix = wgt_C[ix] + (wgt_C .* C_init)' * dM_logP_C_fix[:, ix];
            M_part_fix = (wgt_M[ix] - (wgt_M .* M_init)' * dM_logP_M_fix[:, ix]);
            beta_matrix[3, (i - 1) * 2 + ix] = C_part_fix / M_part_fix;
            beta_ratio_matrix[3, (i - 1) * 2 + ix] = C_part_fix / M_part_fix / wL_init[2] / U_C[ix, ix];
        end
    end

    res_matrix = zeros(size(beta_ratio_matrix)[1] + 1, size(beta_ratio_matrix)[2]);
    res_matrix[1, :] .= vec(m_output[:, 1:2]');
    res_matrix[2:4, :] .= beta_ratio_matrix;
    df_res = DataFrame(
        res_matrix,
        ["pct_empty_CHN", "pct_empty_USA",
        "pct_stock_CHN", "pct_stock_USA",
        "pct_allies_CHN", "pct_allies_USA",
        "pct_total_CHN", "pct_total_USA"]
    );
    df_res = df_res[[1, 2, 4], :];
    CSV.write(joinpath(STATS_PATH, "calibration", "model", "beta", "beta_estimates.csv"), df_res);
end


function estimate_beta(model)
    m_stock = model["m_stock"];
    M_init = model["M_init"];
    C_init = model["C_init"];
    C_real = model["C_real_init"];
    P_M_init = model["P_M_init"];
    P_C_init = model["P_C_init"];
    P_matrix = model["P_matrix"];
    gamma = model["gamma"];

    U_C = model["U_C"];
    dM_logP_C = model["dM_logP_C"];
    dM_logP_M = model["dM_logP_M"];

    # conflict share
    nu = zeros(3, 3);
    for ix in 1:2
        P_vector = P_matrix[:, ix];
        nu_vect = P_vector .* (m_stock + M_init ./ P_M_init) .^ gamma;
        nu_vect = nu_vect ./ sum(nu_vect);
        nu[:, ix] .= nu_vect;
    end

    # estimate beta
    beta = zeros(3);
    for ix in 1:2
        wgt_C = U_C[:, ix] ./ P_C_init;

        wgt_M = -gamma ./ (m_stock .+ M_init ./ P_M_init) .* nu[ix, ix] .* nu[:, ix] ./ P_M_init;
        wgt_M[3] = 0.0;
        wgt_M[ix] = gamma / (m_stock[ix] .+ M_init[ix] ./ P_M_init[ix]) * nu[ix, ix] * (1 - nu[ix, ix]) ./ P_M_init[ix];

        C_part = (wgt_C[ix] + (wgt_C .* C_init)' * dM_logP_C[:, ix]);
        M_part = (wgt_M[ix] - (wgt_M .* M_init)' * dM_logP_M[:, ix]);
        beta[ix] = C_part / M_part;
    end
    if beta_mode == "mult"
        beta = beta_mult .* beta;
    end
    model["beta"] = beta;
    model["beta_init"] = copy(beta);
    model["beta_start"] = copy(beta);

    for s in stock_country
        m_stock[s] = m_stock[s] * stock_mult;
    end
    model["m_stock"] = copy(m_stock);

    nu = zeros(3, 3);
    for ix in 1:2
        P_vector = P_matrix[:, ix];
        nu_vect = P_vector .* (m_stock + M_init ./ P_M_init) .^ gamma;
        nu_vect = nu_vect ./ sum(nu_vect);
        nu[:, ix] .= nu_vect;
    end
    model["nu_init"] = copy(nu);

    # set derivatives
    U_M = zeros(3, 3);
    for ix in 1:2
        wgt_M = -gamma * beta[ix] ./ (m_stock .+ M_init ./ P_M_init) .* nu[ix, ix] .* nu[:, ix] ./ P_M_init;
        wgt_M[3] = 0.0;
        wgt_M[ix] = gamma * beta[ix] / (m_stock[ix] .+ M_init[ix] ./ P_M_init[ix]) * nu[ix, ix] * (1 - nu[ix, ix]) ./ P_M_init[ix];
        U_M[:, ix] .= copy(wgt_M);
    end
    model["U_M_init"] = copy(U_M);

    # set utility
    U_cons_init = U_C' * C_real;
    U_mil_init = Diagonal(nu) * beta;
    U_int_init = Diagonal(U_C) * C_real + U_mil_init;
    U_init = U_cons_init + U_mil_init;
    model["U_cons_init"] = copy(U_cons_init);
    model["U_mil_init"] = copy(U_mil_init);
    model["U_int_init"] = copy(U_int_init);
    model["U_init"] = copy(U_init);
end


function get_LambdaR(model)
    OmegaXP = model["OmegaXP"];
    OmegaL = model["OmegaL"];
    pair_ix = model["pair_ix"];
    tau_X = model["tau_X"];
    tau_M = model["tau_M"];
    LambdaR = zeros(3, size(OmegaXP)[1]);
    for j in 1:3
        for k in 1:3
            if k != j 
                LambdaR[j, pair_ix[k][j]] = LambdaR[j, pair_ix[k][j]] .+ (
                    (tau_X[k][j] .- 1) ./ tau_X[k][j] .* OmegaXP[pair_ix[k][j], j] ./ tau_M[k][j]
                );
                LambdaR[k, pair_ix[k][j]] = LambdaR[k, pair_ix[k][j]] .+ (
                    (tau_M[k][j] .- 1) ./ tau_M[k][j] .* OmegaXP[pair_ix[k][j], j])
            end
            if k == j
                LambdaR[j, pair_ix[k][j]] = (tau_X[k][j] .- 1) ./ tau_X[k][j] .* OmegaL[pair_ix[k][j], j];
            end
        end
    end
    model["LambdaR"] = LambdaR;
    return(LambdaR);
end


function get_LambdaX(model)
    Psi_tilde = model["Psi_tilde"];
    s_C = model["s_C"];
    LambdaR = model["LambdaR"];
    LambdaX = inv(I - Psi_tilde' * s_C * LambdaR);
    model["LambdaX"] = LambdaX;
    return(LambdaX);
end


function get_LambdaL(model)
    Psi_tilde = model["Psi_tilde"];
    s_C = model["s_C"];
    LambdaX = model["LambdaX"];
    OmegaL_tilde = model["OmegaL_tilde"];
    LambdaL = I - OmegaL_tilde' * LambdaX * Psi_tilde' * s_C;
    model["LambdaL"] = LambdaL;
    return(LambdaL);
end


function hat_algebra(model, initial, max_iter = 1_000_000, tol_wage = 1e-5, tol_price = 1e-5, tol_M = 1e-10)
    print_log("Updating hat algebra...")

    # unchanged parameters
    tau_X = model["tau_X"];
    tau_M = model["tau_M"];
    Omega_init = model["Omega_init"];
    OmegaL_init = model["OmegaL_init"];
    pair_ix = model["pair_ix"];
    wL_init = model["wL_init"];
    dict_sigma = model["dict_sigma"];
    set_rest = model["dict_sigma_rest"];
    sigma = model["sigma"];
    s_M = model["s_M"];
    s_C = model["s_C"];
    P_M_init = model["P_M_init"];
    P_C_init = model["P_C_init"];
    firm_set = model["firm_set"];
    D = model["D"];
    U_C = model["U_C"];
    beta = model["beta"];
    dM_logP_M = model["dM_logP_M"];
    dM_logP_C = model["dM_logP_C"];
    P_matrix = model["P_matrix"];
    gamma = model["gamma"];
    m_stock = model["m_stock"];
    M_init = model["M_init"];
    
    # changed parameters
    hat_w = copy(model["hat_w"]);
    hat_p = copy(model["hat_p"]);
    C = copy(model["C"]);
    X = copy(model["X"]);
    R = copy(model["R"]);
    M = copy(model["M"]);
    
    hat_w_guess = copy(hat_w);
    shift_w = ones(3);
    wgt_w = ones(3);
    hat_p_guess = copy(hat_p);
    tau_matrix = ones(size(Omega_init));
    tauL_matrix = ones(size(OmegaL_init));
    for j in 1:3
        for k in 1:3
            if k != j
                ix = findall(firm_set[:, k]);
                tau_matrix[pair_ix[j][k], ix] .= tau_matrix[pair_ix[j][k], ix] .* tau_X[j][k] .* tau_M[j][k];
            end
            if k == j
                tauL_matrix[pair_ix[j][k], j] .= tauL_matrix[pair_ix[j][k], j] .* tau_X[j][k];
            end
        end
    end
    
    # helpers for price iteration
    tax_rest = Omega_init[set_rest, :] .* tau_matrix[set_rest, :] .^ (1 .- sigma[set_rest]);
    dict_sigma_tier1 = [];
    for val in dict_sigma
        sigma_val = val[1];
        node_set = val[2];
        other_set = setdiff(1:size(Omega_init)[1], node_set);

        if sigma_val != 1
            element_inv = inv(I - Omega_init[node_set, node_set] .* tau_matrix[node_set, node_set] .^ (1 - sigma_val));
            element_tax = element_inv * (Omega_init[node_set, other_set] .* tau_matrix[node_set, other_set] .^ (1 - sigma_val));
            element = (element_inv, element_tax);
        else
            element_inv = inv(I - Omega_init[node_set, node_set]);
            element_tax = exp.(element_inv * sum(Omega_init[node_set, :] .* log.(tau_matrix[node_set, :]), dims = 2));
            element_price = element_inv * Omega_init[node_set, other_set];
            element = (element_inv, element_tax, element_price);
        end
        dict_sigma_tier1 = vcat(dict_sigma_tier1, element);
    end

    # iteration
    dist_wage = Inf;
    for iter in 1:max_iter
        print_log("Wage: iteration $iter, distance $dist_wage, min wgt $(minimum(wgt_w))");
        wL = wL_init .* hat_w_guess;

        # helpers for price iteration
        if length(set_rest) > 0
            wage_rest = sum(OmegaL_init[set_rest, :] .* ((tauL_matrix .* hat_w_guess') .^ (1 .- sigma[set_rest])), dims = 2);
        else
            wage_rest = Any[];
        end
        dict_sigma_tier2 = [];
        for (ix, val) in enumerate(dict_sigma)
            sigma_val = val[1];
            node_set = val[2];
            element = dict_sigma_tier1[ix];
            element_inv = element[1];
    
            if sigma_val != 1
                element_wage = element_inv * sum(OmegaL_init[node_set, :] .* ((tauL_matrix[node_set, :] .* hat_w_guess') .^ (1 - sigma_val)), dims = 2);
            else
                element_wage = exp.(element_inv * sum(OmegaL_init[node_set, :] .* (log.(tauL_matrix[node_set, :]) .+ log.(hat_w_guess)'), dims = 2));
            end
            dict_sigma_tier2 = vcat(dict_sigma_tier2, (sigma_val, element_wage));
        end

        # price iteration loop
        dist_price = Inf;
        for j in 1:max_iter
            # print_log("Price: iteration $j, distance $dist_price")
            if length(set_rest) > 0
                hat_p[set_rest] .= (
                    wage_rest + sum(tax_rest .* hat_p_guess' .^ (1 .- sigma[set_rest]), dims = 2)
                ) .^ (1 ./ (1 .- sigma[set_rest]));
            end
            for (ix, val) in enumerate(dict_sigma)
                sigma_val = val[1];
                node_set = val[2];
                other_set = setdiff(1:size(Omega_init)[1], node_set);

                element_tax = dict_sigma_tier1[ix][2];
                element_wage = dict_sigma_tier2[ix][2];

                if sigma_val != 1
                    hat_p[node_set] .= (element_wage + element_tax * hat_p[other_set] .^ (1 - sigma_val)) .^ (1 / (1 - sigma_val));
                else
                    element_price = dict_sigma_tier1[ix][3];
                    hat_p[node_set] .= element_tax .* element_wage .* exp.(element_price * log.(hat_p[other_set]));
                end
            end
            
            dist_price = maximum(abs.(hat_p .- hat_p_guess));
            if dist_price < tol_price
                # print_log("Prices converged on iteration $j, distance $dist_price")
                break
            end
            hat_p_guess = copy(hat_p);
        end
        
        # price indices
        hat_P_M = s_M' * hat_p;
        hat_P_C = s_C' * hat_p;
        P_M = hat_P_M .* P_M_init;
        P_C = hat_P_C .* P_C_init;

        # shares changes
        hat_Omega = (tau_matrix .* hat_p' ./ hat_p) .^ (1 .- sigma);
        Omega = Omega_init .* hat_Omega;
        Omega_tilde = Omega ./ tau_matrix;
        Psi = inv(I - Omega);
        Psi[Psi .< 0] .= 0;
        Psi_tilde = inv(I - Omega_tilde);
        Psi_tilde[Psi_tilde .< 0] .= 0;
        OmegaLOne = 1 .- sum(Omega, dims = 2);
        OmegaLOne[OmegaLOne .<= 1e-12] .= 0;
        OmegaL = OmegaLOne .* firm_set;
        OmegaL = max.(OmegaL, 0);
        OmegaL_tilde = OmegaL ./ tauL_matrix;
        OmegaXP = zeros(size(Omega)[1], 3);
        for j in 1:3
            OmegaXP[:, j] = sum(firm_set[:, j]' .* Omega, dims = 2);
        end

        # update military
        if ((mil_mode == "act_mil") || ((mil_mode == "fix_mil" || mil_mode == "tax_mil") && (beta_mode == "mult" || beta_mode == "stock") && initial && stock_rebalance))
            # print_log(model["M_init"]);
            # print_log(model["M"]);
            M_guess = copy(M);
            M_new = copy(M_guess);
            C_guess = copy(C);
            M_new = zeros(3);

            dist_M = Inf;
            for j in 1:max_iter    
                # conflict share
                nu = zeros(3, 3);
                for ix in 1:2
                    P_vector = P_matrix[:, ix];
                    nu_vect = P_vector .* (m_stock + M_guess ./ P_M) .^ gamma;
                    nu_vect = nu_vect ./ sum(nu_vect);
                    nu[:, ix] .= nu_vect;
                end

                # estimate beta
                for ix in 1:2
                    wgt_C = U_C[:, ix] ./ P_C;
                    wgt_M = -gamma * beta[ix] * nu[ix, ix] .* nu[:, ix] ./ P_M;
                    wgt_M[ix] = gamma * beta[ix] * nu[ix, ix] * (1 - nu[ix, ix]) ./ P_M[ix];

                    C_part = (wgt_C[ix] + (wgt_C .* C_guess)' * dM_logP_C[:, ix]);
                    M_part = (wgt_M[ix] - (wgt_M .* M_guess)' * dM_logP_M[:, ix]);
                    M_new[ix] = (M_part / C_part - m_stock[ix]) .* P_M[ix];
                end
                
                dist_M = maximum(abs.(M_new .- M_guess));
                if dist_M < tol_M
                    # print_log("Military converged on iteration $j, distance $dist_M")
                    M = copy(M_guess);
                    model["M"] = copy(M);
                    break
                end
                M_guess = copy(M_new);
                C_guess = wL + R + D - M_guess;
            end
        end
        if (mil_mode == "tax_mil")
            M = (wL + R + D) .* (M_init ./ (wL_init + D));
            model["M"] = copy(M);
        end
        
        # update matrices necessary for updating
        model["OmegaXP"] = copy(OmegaXP);
        model["Psi_tilde"] = copy(Psi_tilde);
        model["OmegaL"] = copy(OmegaL);
        LambdaR = get_LambdaR(model);
        LambdaX = get_LambdaX(model);
        LambdaL = get_LambdaL(model);

        # solve for the wage
        block = LambdaX * Psi_tilde';
        A = LambdaL;
        b = OmegaL_tilde' * block * (s_C * (D - M) + s_M * M);
        A_small = A[1:2, 1:2];
        b_small = b[1:2] - [A[1, 3] * wL_init[3], A[2, 3] * wL_init[3]];
        prob_small = LinearProblem(A_small, b_small, u0 = wL[1:2]);
        sol_small = solve(prob_small);
        wL = copy(wL_init);
        wL[1] = sol_small.u[1]
        wL[2] = sol_small.u[2];
        hat_w = wL ./ wL_init;

        dist_wage = maximum(abs.(hat_w .- hat_w_guess));
        check = sign.(hat_w - hat_w_guess) .== sign.(shift_w);
        check[3] = Bool(1);
        shift_w = hat_w - hat_w_guess;
        wgt_w[check] .= min.(wgt_w[check] .* 2.0, ones(length(findall(check))));
        wgt_w[.!check] .= wgt_w[.!check] .* 0.5;
        hat_w_guess = (hat_w_guess .^ (1 .- wgt_w)) .* (hat_w .^ wgt_w);

        if initial && beta_mode == "mult"
            get_dlogw_dP(model, true);
        end

        if dist_wage < tol_wage
            print_log("Wages converged on iteration $iter, distance $dist_wage");

            # update the model
            model["hat_p"] = copy(hat_p);
            model["P_M"] = copy(P_M);
            model["P_C"] = copy(P_C);
            model["hat_P_M"] = copy(hat_P_M);
            model["hat_P_C"] = copy(hat_P_C);
            model["wL"] = copy(wL);
            model["hat_w"] = copy(hat_w_guess);
            model["Omega"] = copy(Omega);
            model["Omega_tilde"] = copy(Omega_tilde);
            model["OmegaL"] = copy(OmegaL);
            model["OmegaL_tilde"] = copy(OmegaL_tilde);
            model["Psi"] = copy(Psi);

            # consumption
            X = block * (s_C * (wL + D - M) + s_M * M);
            R = LambdaR * X;
            C = wL + R + D - M;
            C_real = C ./ P_C;
            M_real = M ./ P_M;

            nu = zeros(3, 3);
            U_M = zeros(3, 3);
            if mil_mode != "no_mil"
                # conflict share
                for ix in 1:2
                    P_vector = P_matrix[:, ix];
                    nu_vect = P_vector .* (m_stock + M ./ P_M) .^ gamma;
                    nu_vect = nu_vect ./ sum(nu_vect);
                    nu[:, ix] .= nu_vect;
                end
                model["nu"] = copy(nu);

                # set derivatives
                for ix in 1:2
                    wgt_M = -gamma * model["beta"][ix] ./ (m_stock .+ M ./ P_M) .* nu[ix, ix] .* nu[:, ix] ./ P_M;
                    wgt_M[3] = 0.0;
                    wgt_M[ix] = gamma * model["beta"][ix] / (m_stock[ix] .+ M[ix] ./ P_M[ix]) * nu[ix, ix] * (1 - nu[ix, ix]) ./ P_M[ix];
                    U_M[:, ix] .= copy(wgt_M);
                end
                model["U_M"] = copy(U_M);
            end

            # utility
            U_cons = U_C' * C_real;
            U_mil = Diagonal(nu) * model["beta"];
            U_int = Diagonal(U_C) * C_real + U_mil;
            U = U_cons + U_mil;
            model["U_cons"] = copy(U_cons);
            model["U_mil"] = copy(U_mil);
            model["U_int"] = copy(U_int);
            model["U"] = copy(U);

            # update the model
            model["wL"] = copy(wL);
            model["hat_w"] = copy(hat_w_guess);
            model["R"] = copy(R);
            model["C"] = copy(C);
            model["C_real"] = copy(C_real);
            model["M"] = copy(M);
            model["M_real"] = copy(M_real);
            model["X"] = copy(X);
            OmegaXP = zeros(size(Omega)[1], 3);
            for j in 1:3
                OmegaXP[:, j] = sum(firm_set[:, j]' .* Omega, dims = 2);
            end
            model["OmegaXP"] = copy(OmegaXP);
            model["F"] = X .* OmegaXP;
            MP = zeros(3, 3);
            for i in 1:3
                for j in 1:3
                    MP[i, j] = model["F"][:, i]' * firm_set[:, j];
                end
            end
            model["MP"] = copy(MP);

            model["cent_M"] = (s_M' * Psi)';
            model["cent_C"] = (s_C' * Psi)';
            model["adj_cent_M"]= model["cent_M"] ./ sum(model["cent_M"], dims = 1) * 100;
            model["adj_cent_C"] = model["cent_C"] ./ sum(model["cent_C"], dims = 1) * 100;
            model["E"] = s_M .* M' + s_C .* C';
            model["E_M"] = s_M .* M';
            model["E_C"] = s_C .* C';
            E_rob = copy(model["E"]);
            E_rob[E_rob .<= 0] .= 1;
            model["S_M"] = s_M ./ E_rob .* M';
            model["S_C"] = s_C ./ E_rob .* C';
            check_S = model["S_M"] + model["S_C"];
            # @assert maximum(abs.(check_S[check_S .!= 0] .- 1.0)) < 1e-14;
            model["C_D"] = Psi' * model["E"] ./ X;
            model["C_M"] = Psi' * (s_M .* M') ./ X;
            model["C_C"] = Psi' * (s_C .* C') ./ X;
            model["C_D_sigma"] = model["C_D"] ./ model["sigma"];
            model["C_M_sigma"] = model["C_M"] ./ model["sigma"];
            model["C_C_sigma"] = model["C_C"] ./ model["sigma"];

            if initial
                model["OmegaXP_init"] = copy(model["OmegaXP"]);
                model["Psi_tilde_init"] = copy(model["Psi_tilde"]);
                model["OmegaL_init"] = copy(model["OmegaL"]);
                model["OmegaL_tilde_init"] = copy(model["OmegaL_tilde"]);

                model["hat_p_init"] = copy(model["hat_p"]);
                model["P_M_init"] = copy(model["P_M"]);
                model["P_C_init"] = copy(model["P_C"]);
                model["hat_P_M_init"] = copy(model["hat_P_M"]);
                model["hat_P_C_init"] = copy(model["hat_P_C"]);
                model["Omega_init"] = copy(model["Omega"]);
                model["Omega_tilde_init"] = copy(model["Omega_tilde"]);
                model["Psi_init"] = copy(model["Psi"]);

                model["M_init"] = copy(model["M"]);
                model["nu_init"] = copy(model["nu"]);
                model["U_M_init"] = copy(model["U_M"]);
                model["M_real_init"] = copy(model["M_real"]);

                model["U_cons_init"] = copy(model["U_cons"]);
                model["U_mil_init"] = copy(model["U_mil"]);
                model["U_int_init"] = copy(model["U_int"]);
                model["U_init"] = copy(model["U"]);

                model["wL_init"] = copy(model["wL"]);
                model["hat_w_init"] = copy(model["hat_w"]);
                model["R_init"] = copy(model["R"]);
                model["C_init"] = copy(model["C"]);
                model["C_real_init"] = copy(model["C_real"]);
                model["X_init"] = copy(model["X"]);

                model["OmegaXP_init"] = copy(model["OmegaXP"]);
                model["F_init"] = copy(model["F"]);
                model["MP_init"] = copy(model["MP"]);

                model["cent_M_init"] = copy(model["cent_M"]);
                model["cent_C_init"] = copy(model["cent_C"]);
                model["adj_cent_M_init"] = copy(model["adj_cent_M"]);
                model["adj_cent_C_init"] = copy(model["adj_cent_C"]);
                model["E_init"] = copy(model["E"]);
                model["E_M_init"] = copy(model["E_M"]);
                model["E_C_init"] = copy(model["E_C"]);
                model["S_M_init"] = copy(model["S_M"]);
                model["S_C_init"] = copy(model["S_C"]);
                model["C_D_init"] = copy(model["C_D"]);
                model["C_M_init"] = copy(model["C_M"]);
                model["C_C_init"] = copy(model["C_C"]);
                model["C_D_sigma_init"] = copy(model["C_D_sigma"]);
                model["C_M_sigma_init"] = copy(model["C_M_sigma"]);
                model["C_C_sigma_init"] = copy(model["C_C_sigma"]);
            end
            return dist_wage, iter;
        end
    end
end


function solve_taxes(ix_imp, ix_exp, model, export_flow = true, initial = false) 
    print_log("Finding optimal tax: importer $ix_imp, exporter $ix_exp")

    if export_flow
        suffix = "";
    else
        suffix = "M";
    end

    # invert elasticity
    pair_ix = model["pair_ix"];
    if ix_imp != ix_exp
        elast = (
            model["F"][pair_ix[ix_imp][ix_exp], ix_exp] ./ model["tau_M"][ix_imp][ix_exp]
            .* model["dT" * suffix * "_logF"][ix_imp][ix_exp]
        );
    else
        elast = model["FL"][pair_ix[ix_imp][ix_exp], ix_exp] .* model["dT" * suffix * "_logF"][ix_imp][ix_exp];
    end
    elast_inv = -inv(elast');

    # model weights
    U_C = model["U_C"];
    wgt_C = U_C[:, ix_exp] ./ model["P_C"];
    wgt_C_adj = wgt_C ./ wgt_C[ix_exp];
    U_M = model["U_M"];
    wgt_M = U_M[:, ix_exp] ./ model["P_M"];
    wgt_M_adj = wgt_M ./ wgt_C[ix_exp];

    # revenue
    if ix_imp != ix_exp
        model["T" * suffix * "_rev"][ix_imp][ix_exp] .= elast_inv * model["F"][pair_ix[ix_imp][ix_exp], ix_exp] ./ model["tau_M"][ix_imp][ix_exp];
    else
        model["T" * suffix * "_rev"][ix_imp][ix_exp] .= elast_inv * model["FL"][pair_ix[ix_imp][ix_exp], ix_exp];
    end
  
    # income GE wage
    wage_GE = vec(wgt_C_adj' * (model["wL"] .* model["dT" * suffix * "_logw"][ix_imp][ix_exp]));
    model["T" * suffix * "_wage"][ix_imp][ix_exp] .= elast_inv * wage_GE;

    # income GE revenue
    dT_R = copy(model["dT" * suffix * "_R"][ix_imp][ix_exp]);
    rev_row = vec(wgt_C_adj' * dT_R);
    model["T" * suffix * "_rev_row"][ix_imp][ix_exp] .= elast_inv * rev_row;

    # price PE consumption
    price_C_PE = -vec(wgt_C_adj' * (model["C"] .* model["dT" * suffix * "_logP_C_tax"][ix_imp][ix_exp]));
    model["T" * suffix * "_price_C_PE"][ix_imp][ix_exp] .= elast_inv * price_C_PE;

    # price PE military
    price_M_PE = -vec(wgt_M_adj' * (model["M"] .* model["dT" * suffix * "_logP_M_tax"][ix_imp][ix_exp]));
    model["T" * suffix * "_price_M_PE"][ix_imp][ix_exp] .= elast_inv * price_M_PE;

    # price GE consumption
    price_C_GE = -vec(wgt_C_adj' * (model["C"] .* model["dT" * suffix * "_logP_C_wage"][ix_imp][ix_exp]));
    model["T" * suffix * "_price_C_GE"][ix_imp][ix_exp] .= elast_inv * price_C_GE;

    # price GE military
    price_M_GE = -vec(wgt_M_adj' * (model["M"] .* model["dT" * suffix * "_logP_M_wage"][ix_imp][ix_exp]));
    model["T" * suffix * "_price_M_GE"][ix_imp][ix_exp] .= elast_inv * price_M_GE;

    # identities
    model["T" * suffix][ix_imp][ix_exp] .= (
        model["T" * suffix * "_rev"][ix_imp][ix_exp]
        + model["T" * suffix * "_wage"][ix_imp][ix_exp] + model["T" * suffix * "_rev_row"][ix_imp][ix_exp]
        + model["T" * suffix * "_price_C_PE"][ix_imp][ix_exp] + model["T" * suffix * "_price_M_PE"][ix_imp][ix_exp]
        + model["T" * suffix * "_price_C_GE"][ix_imp][ix_exp] + model["T" * suffix * "_price_M_GE"][ix_imp][ix_exp]
    );
    if ix_exp == ix_imp
        model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .<= -9] .= -9;
        model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .>= 1.0 - 1e-1] .= 1.0 - 1e-1;
        lambda_mult = model["FL"][pair_ix[ix_imp][ix_exp], ix_exp]' * model["T"][ix_imp][ix_exp] / (model["FL"][pair_ix[ix_imp][ix_exp], ix_exp]' * elast_inv * model["FL"][pair_ix[ix_imp][ix_exp], ix_exp]);
        model["T" * suffix][ix_imp][ix_exp] = model["T" * suffix][ix_imp][ix_exp] - lambda_mult * elast_inv * model["FL"][pair_ix[ix_imp][ix_exp], ix_exp];
        @assert abs(model["FL"][pair_ix[ix_imp][ix_exp], ix_exp]' * model["T" * suffix][ix_imp][ix_exp]) <= 1e-10;
        model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .>= 1.0 - 1e-1] .= 1.0 - 1e-1;
        model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .<= -9] .= -9;
    end

    model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .>= 1.0 - 1e-3] .= 1.0 - 1e-3;
    model["T" * suffix][ix_imp][ix_exp][model["T" * suffix][ix_imp][ix_exp] .<= -9] .= -9;
    model["T" * suffix * "_income"][ix_imp][ix_exp] = model["T" * suffix * "_wage"][ix_imp][ix_exp] + model["T" * suffix * "_rev_row"][ix_imp][ix_exp];
    model["T" * suffix * "_price_C"][ix_imp][ix_exp] = model["T" * suffix * "_price_C_PE"][ix_imp][ix_exp] + model["T" * suffix * "_price_C_GE"][ix_imp][ix_exp];
    model["T" * suffix * "_price_M"][ix_imp][ix_exp] = model["T" * suffix * "_price_M_PE"][ix_imp][ix_exp] + model["T" * suffix * "_price_M_GE"][ix_imp][ix_exp];
    model["T" * suffix * "_price_PE"][ix_imp][ix_exp] = model["T" * suffix * "_price_C_PE"][ix_imp][ix_exp] + model["T" * suffix * "_price_M_PE"][ix_imp][ix_exp];
    model["T" * suffix * "_price_GE"][ix_imp][ix_exp] = model["T" * suffix * "_price_C_GE"][ix_imp][ix_exp] + model["T" * suffix * "_price_M_GE"][ix_imp][ix_exp];
    T = copy(model["T" * suffix][ix_imp][ix_exp]);
    t = 1.0 ./ (1.0 .- T);
    
    if export_flow
        wgt_check = sign.((t .- model["tau_X"][ix_imp][ix_exp])) .== sign.(model["delta_T"][ix_imp][ix_exp]);
    else 
        wgt_check = sign.((t .- model["tau_M"][ix_imp][ix_exp])) .== sign.(model["delta_TM"][ix_imp][ix_exp]);
    end
    wgt_mult = 0.5 * ones(length(wgt_check));
    wgt_mult[wgt_check .> 0.5] .= 2.0;
    model["wgt_T" * suffix][ix_imp][ix_exp] .= min.(copy(model["wgt_T" * suffix][ix_imp][ix_exp]) .* wgt_mult, ones(length(wgt_check)));
    if export_flow
        model["delta_T"][ix_imp][ix_exp] .= t .- model["tau_X"][ix_imp][ix_exp];
        model["tau_X"][ix_imp][ix_exp] .= (
            copy(model["tau_X"][ix_imp][ix_exp]) .^ (1 .- model["wgt_T"][ix_imp][ix_exp]) .* t .^ model["wgt_T"][ix_imp][ix_exp]
        );
    else
        model["delta_TM"][ix_imp][ix_exp] .= t .- model["tau_M"][ix_imp][ix_exp];
        model["tau_M"][ix_imp][ix_exp] .= (
            copy(model["tau_M"][ix_imp][ix_exp]) .^ (1 .- model["wgt_TM"][ix_imp][ix_exp]) .* t .^ model["wgt_TM"][ix_imp][ix_exp]
        );
    end

    if initial
        model["T" * suffix * "_init"][ix_imp][ix_exp] .= copy(model["T" * suffix][ix_imp][ix_exp]);
        model["T" * suffix * "_rev_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_rev"][ix_imp][ix_exp]);
        model["T" * suffix * "_wage_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_wage"][ix_imp][ix_exp]);
        model["T" * suffix * "_rev_row_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_rev_row"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_C_PE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_C_PE"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_C_GE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_C_GE"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_M_PE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_M_PE"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_M_GE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_M_GE"][ix_imp][ix_exp]);
        model["T" * suffix * "_income_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_income"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_C_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_C"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_M_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_M"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_PE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_PE"][ix_imp][ix_exp]);
        model["T" * suffix * "_price_GE_init"][ix_imp][ix_exp] .= copy(model["T" * suffix * "_price_GE"][ix_imp][ix_exp]);
        if export_flow
            model["tau_X_init"][ix_imp][ix_exp] .= copy(model["tau_X"][ix_imp][ix_exp]);
        else
            model["tau_M_init"][ix_imp][ix_exp] .= copy(model["tau_M"][ix_imp][ix_exp]);
        end
    end

    sum_tau = zeros(3);
    sum_F = zeros(3);
    for pair in active_pairs
        sum_tau[ix_exp] = sum_tau[ix_exp] + sum(model["tau_X"][ix_imp][ix_exp] .* model["F_init"][pair_ix[ix_imp][ix_exp], ix_exp]);
        sum_F[ix_exp] = sum_F[ix_exp] + sum(model["F_init"][pair_ix[ix_imp][ix_exp], ix_exp]);
    end
    for pair in active_import_pairs
        sum_tau[ix_imp] = sum_tau[ix_imp] + sum(model["tau_M"][ix_imp][ix_exp] .* model["F_init"][pair_ix[ix_imp][ix_exp], ix_exp]);
        sum_F[ix_imp] = sum_F[ix_imp] + sum(model["F_init"][pair_ix[ix_imp][ix_exp], ix_exp]);
    end
    model["tau_avg"] = sum_tau ./ sum_F;

    if export_flow
        min_T = minimum(model["tau_X"][ix_imp][ix_exp]);
        max_T = maximum(model["tau_X"][ix_imp][ix_exp]);
        mean_T = sum(model["tau_X"][ix_imp][ix_exp]) / length(model["tau_X"][ix_imp][ix_exp]);
    else
        min_T = minimum(model["tau_M"][ix_imp][ix_exp]);
        max_T = maximum(model["tau_M"][ix_imp][ix_exp]);
        mean_T = sum(model["tau_M"][ix_imp][ix_exp]) / length(model["tau_M"][ix_imp][ix_exp]);
    end
    sum_pos = sum(model["delta_T" * suffix][ix_imp][ix_exp] .> 0.0);
    sum_tot = length(model["delta_T" * suffix][ix_imp][ix_exp]);
    max_chg = maximum(model["delta_T" * suffix][ix_imp][ix_exp]);
    min_chg = minimum(model["delta_T" * suffix][ix_imp][ix_exp]);
    mean_chg = sum(model["delta_T" * suffix][ix_imp][ix_exp]) / sum_tot;
    min_wgt = minimum(model["wgt_T" * suffix][ix_imp][ix_exp]);
    max_wgt = maximum(model["wgt_T" * suffix][ix_imp][ix_exp]);
    mean_wgt = sum(model["wgt_T" * suffix][ix_imp][ix_exp]) / length(model["wgt_T" * suffix][ix_imp][ix_exp]);
    print_log("*** TAX ITERATION STATS ***");
    print_log("TAX min: $min_T, max: $max_T, mean: $mean_T");
    print_log("CHG min: $min_chg, max: $max_chg, mean: $mean_chg, pos: $sum_pos out of $sum_tot");
    print_log("WGT min: $min_wgt, max: $max_wgt, mean: $mean_wgt");
end


function write_model(model, name)
    attrib = copy(model["attrib"]);

    function add_tax_columns(name, pair1, pair2, array)
        attrib[!, name] = fill(missing, nrow(attrib));
        attrib[!, name] = convert(Vector{Union{Missing, Float64}}, attrib[!, name]);
        for (k, ix) in enumerate(model["pair_ix"][pair1][pair2])
            attrib[ix, name] = array[k];
        end
    end

    # add macro columns
    cty_list = ["CHN", "USA", "ROW"];
    for i = 1:3
        # budget constraint
        attrib[!, "C_init_" * cty_list[i]] .= model["C_init"][i];
        attrib[!, "C_" * cty_list[i]] .= model["C"][i];
        attrib[!, "delta_C_" * cty_list[i]] .= (attrib[!, "C_" * cty_list[i]] - attrib[!, "C_init_" * cty_list[i]]) ./ attrib[!, "C_init_" * cty_list[i]] .* 100;
        attrib[!, "wL_init_" * cty_list[i]] .= model["wL_init"][i];
        attrib[!, "wL_" * cty_list[i]] .= model["wL"][i];
        attrib[!, "delta_wL_" * cty_list[i]] .= (attrib[!, "wL_" * cty_list[i]] - attrib[!, "wL_init_" * cty_list[i]]) ./ attrib[!, "wL_init_" * cty_list[i]] .* 100;
        attrib[!, "R_init_" * cty_list[i]] .= 0.0;
        attrib[!, "R_" * cty_list[i]] .= model["R"][i];
        attrib[!, "delta_R_" * cty_list[i]] .= (attrib[!, "R_" * cty_list[i]] - attrib[!, "wL_" * cty_list[i]]) ./ attrib[!, "wL_" * cty_list[i]] .* 100;
        attrib[!, "D_init_" * cty_list[i]] .= model["D"][i];
        attrib[!, "D_" * cty_list[i]] .= model["D"][i];
        attrib[!, "M_init_" * cty_list[i]] .= model["M_init"][i];
        attrib[!, "M_" * cty_list[i]] .= model["M"][i];
        attrib[!, "delta_M_" * cty_list[i]] .= (attrib[!, "M_" * cty_list[i]] - attrib[!, "M_init_" * cty_list[i]]) ./ attrib[!, "M_init_" * cty_list[i]] .* 100;
        attrib[!, "MP_init_CHN_" * cty_list[i]] .= model["MP_init"][1, i];
        attrib[!, "MP_init_USA_" * cty_list[i]] .= model["MP_init"][2, i];
        attrib[!, "MP_init_ROW_" * cty_list[i]] .= model["MP_init"][3, i];
        attrib[!, "MP_CHN_" * cty_list[i]] .= model["MP"][1, i];
        attrib[!, "MP_USA_" * cty_list[i]] .= model["MP"][2, i];
        attrib[!, "MP_ROW_" * cty_list[i]] .= model["MP"][3, i];
        attrib[!, "delta_MP_CHN_" * cty_list[i]] .= (attrib[!, "MP_CHN_" * cty_list[i]] - attrib[!, "MP_init_CHN_" * cty_list[i]]) ./ attrib[!, "MP_init_CHN_" * cty_list[i]] .* 100;
        attrib[!, "delta_MP_USA_" * cty_list[i]] .= (attrib[!, "MP_USA_" * cty_list[i]] - attrib[!, "MP_init_USA_" * cty_list[i]]) ./ attrib[!, "MP_init_USA_" * cty_list[i]] .* 100;
        attrib[!, "delta_MP_ROW_" * cty_list[i]] .= (attrib[!, "MP_ROW_" * cty_list[i]] - attrib[!, "MP_init_ROW_" * cty_list[i]]) ./ attrib[!, "MP_init_ROW_" * cty_list[i]] .* 100;
        attrib[!, "tau_avg_" * cty_list[i]] .= model["tau_avg"][i];
        attrib[!, "delta_tau_avg_" * cty_list[i]] .= (model["tau_avg"][i] - 1) * 100;

        # prices
        attrib[!, "P_C_init_" * cty_list[i]] .= model["P_C_init"][i];
        attrib[!, "P_C_" * cty_list[i]] .= model["P_C"][i];
        attrib[!, "delta_P_C_" * cty_list[i]] .= (attrib[!, "P_C_" * cty_list[i]] - attrib[!, "P_C_init_" * cty_list[i]]) ./ attrib[!, "P_C_" * cty_list[i]] .* 100;
        attrib[!, "P_M_init_" * cty_list[i]] .= model["P_M_init"][i];
        attrib[!, "P_M_" * cty_list[i]] .= model["P_M"][i];
        attrib[!, "delta_P_M_" * cty_list[i]] .= (attrib[!, "P_M_" * cty_list[i]] - attrib[!, "P_M_init_" * cty_list[i]]) ./ attrib[!, "P_M_" * cty_list[i]] .* 100;
        attrib[!, "hat_w_init_" * cty_list[i]] .= model["hat_w_init"][i];
        attrib[!, "hat_w_" * cty_list[i]] .= model["hat_w"][i];
        attrib[!, "delta_hat_w" * cty_list[i]] .= (attrib[!, "hat_w_" * cty_list[i]] - attrib[!, "hat_w_init_" * cty_list[i]]) ./ attrib[!, "hat_w_init_" * cty_list[i]] .* 100;
        attrib[!, "hat_p_init"] .= model["hat_p_init"];
        attrib[!, "hat_p"] .= model["hat_p"];

        # real variables
        attrib[!, "c_init_" * cty_list[i]] .= model["C_real_init"][i];
        attrib[!, "c_" * cty_list[i]] .= model["C_real"][i];
        attrib[!, "delta_c_" * cty_list[i]] .= (attrib[!, "c_" * cty_list[i]] - attrib[!, "c_init_" * cty_list[i]]) ./ attrib[!, "c_init_" * cty_list[i]] .* 100;
        attrib[!, "m_init_" * cty_list[i]] .= model["M_real_init"][i];
        attrib[!, "m_" * cty_list[i]] .= model["M_real"][i];
        attrib[!, "delta_m_" * cty_list[i]] .= (attrib[!, "m_" * cty_list[i]] - attrib[!, "m_init_" * cty_list[i]]) ./ attrib[!, "m_init_" * cty_list[i]] .* 100;
    
        # utility
        attrib[!, "U_start_" * cty_list[i]] .= model["U_start"][i];
        attrib[!, "U_init_" * cty_list[i]] .= model["U_init"][i];
        attrib[!, "U_" * cty_list[i]] .= model["U"][i];
        attrib[!, "delta_U_" * cty_list[i]] .= (attrib[!, "U_" * cty_list[i]] - attrib[!, "U_init_" * cty_list[i]]) ./ attrib[!, "U_init_" * cty_list[i]] .* 100;
        attrib[!, "U_cons_start_" * cty_list[i]] .= model["U_cons_start"][i];
        attrib[!, "U_cons_init_" * cty_list[i]] .= model["U_cons_init"][i];
        attrib[!, "U_cons_" * cty_list[i]] .= model["U_cons"][i];
        attrib[!, "delta_U_cons_" * cty_list[i]] .= (attrib[!, "U_cons_" * cty_list[i]] - attrib[!, "U_cons_init_" * cty_list[i]]) ./ attrib[!, "U_cons_init_" * cty_list[i]] .* 100;
        attrib[!, "U_mil_start_" * cty_list[i]] .= model["U_mil_start"][i];
        attrib[!, "U_mil_init_" * cty_list[i]] .= model["U_mil_init"][i];
        attrib[!, "U_mil_" * cty_list[i]] .= model["U_mil"][i];
        attrib[!, "delta_U_mil_" * cty_list[i]] .= (attrib[!, "U_mil_" * cty_list[i]] - attrib[!, "U_mil_init_" * cty_list[i]]) ./ attrib[!, "U_mil_init_" * cty_list[i]] .* 100;
        attrib[!, "U_int_start_" * cty_list[i]] .= model["U_int_start"][i];
        attrib[!, "U_int_init_" * cty_list[i]] .= model["U_int_init"][i];
        attrib[!, "U_int_" * cty_list[i]] .= model["U_int"][i];
        attrib[!, "delta_U_int_" * cty_list[i]] .= (attrib[!, "U_int_" * cty_list[i]] - attrib[!, "U_int_init_" * cty_list[i]]) ./ attrib[!, "U_int_init_" * cty_list[i]] .* 100;
        attrib[!, "beta_start_" * cty_list[i]] .= model["beta_start"][i];
        attrib[!, "beta_init_" * cty_list[i]] .= model["beta_init"][i];
        attrib[!, "beta_" * cty_list[i]] .= model["beta"][i];
        attrib[!, "delta_beta_" * cty_list[i]] .= (attrib[!, "beta_init_" * cty_list[i]] ./ attrib[!, "wL_init_" * cty_list[i]]) .* 100;
        attrib[!, "nu_init_CHN_" * cty_list[i]] .= model["nu_init"][1, i];
        attrib[!, "nu_init_USA_" * cty_list[i]] .= model["nu_init"][2, i];
        attrib[!, "nu_init_ROW_" * cty_list[i]] .= model["nu_init"][3, i];
        attrib[!, "nu_CHN_" * cty_list[i]] .= model["nu"][1, i];
        attrib[!, "nu_USA_" * cty_list[i]] .= model["nu"][2, i];
        attrib[!, "nu_ROW_" * cty_list[i]] .= model["nu"][3, i];
        attrib[!, "delta_nu_CHN_" * cty_list[i]] .= (attrib[!, "nu_CHN_" * cty_list[i]] - attrib[!, "nu_init_CHN_" * cty_list[i]]) .* 100;
        attrib[!, "delta_nu_USA_" * cty_list[i]] .= (attrib[!, "nu_USA_" * cty_list[i]] - attrib[!, "nu_init_USA_" * cty_list[i]]) .* 100;
        attrib[!, "delta_nu_ROW_" * cty_list[i]] .= (attrib[!, "nu_ROW_" * cty_list[i]] - attrib[!, "nu_init_ROW_" * cty_list[i]]) .* 100;
        attrib[!, "nu_self_init_" * cty_list[i]] .= diag(model["nu_init"])[i];
        attrib[!, "nu_self_" * cty_list[i]] .= diag(model["nu"])[i];
        attrib[!, "delta_nu_self_" * cty_list[i]] .= (attrib[!, "nu_self_" * cty_list[i]] - attrib[!, "nu_self_init_" * cty_list[i]]) .* 100;
        attrib[!, "U_C_CHN_" * cty_list[i]] .= model["U_C"][1, i];
        attrib[!, "U_C_USA_" * cty_list[i]] .= model["U_C"][2, i];
        attrib[!, "U_C_ROW_" * cty_list[i]] .= model["U_C"][3, i];
        attrib[!, "U_M_start_CHN_" * cty_list[i]] .= model["U_M_start"][1, i];
        attrib[!, "U_M_start_USA_" * cty_list[i]] .= model["U_M_start"][2, i];
        attrib[!, "U_M_start_ROW_" * cty_list[i]] .= model["U_M_start"][3, i];
        attrib[!, "U_M_init_CHN_" * cty_list[i]] .= model["U_M_init"][1, i];
        attrib[!, "U_M_init_USA_" * cty_list[i]] .= model["U_M_init"][2, i];
        attrib[!, "U_M_init_ROW_" * cty_list[i]] .= model["U_M_init"][3, i];
        attrib[!, "U_M_CHN_" * cty_list[i]] .= model["U_M"][1, i];
        attrib[!, "U_M_USA_" * cty_list[i]] .= model["U_M"][2, i];
        attrib[!, "U_M_ROW_" * cty_list[i]] .= model["U_M"][3, i];

        # good-level
        attrib[!, "sigma"] = model["sigma"];
        attrib[!, "X_init"] = model["X_init"];
        attrib[!, "X"] = model["X"];
        attrib[!, "s_C_" * cty_list[i]] .= model["s_C"][:, i];
        attrib[!, "s_M_" * cty_list[i]] .= model["s_M"][:, i];
        attrib[!, "E_init_" * cty_list[i]] .= model["E_init"][:, i];
        attrib[!, "E_" * cty_list[i]] .= model["E"][:, i];
        attrib[!, "E_C_init_" * cty_list[i]] .= model["E_C_init"][:, i];
        attrib[!, "E_C_" * cty_list[i]] .= model["E_C"][:, i];
        attrib[!, "E_M_init_" * cty_list[i]] .= model["E_M_init"][:, i];
        attrib[!, "E_M_" * cty_list[i]] .= model["E_M"][:, i];
        attrib[!, "F_init_" * cty_list[i]] .= model["F_init"][:, i];
        attrib[!, "F_" * cty_list[i]] .= model["F"][:, i];
        attrib[!, "OmegaXP_init_" * cty_list[i]] .= model["OmegaXP_init"][:, i];
        attrib[!, "OmegaXP_" * cty_list[i]] .= model["OmegaXP"][:, i];
     
        # centrality
        attrib[!, "C_D_init_" * cty_list[i]] .= model["C_D_init"][:, i];
        attrib[!, "C_D_" * cty_list[i]] .= model["C_D"][:, i];
        attrib[!, "C_C_init_" * cty_list[i]] .= model["C_C_init"][:, i];
        attrib[!, "C_C_" * cty_list[i]] .= model["C_C"][:, i];
        attrib[!, "C_M_init_" * cty_list[i]] .= model["C_M_init"][:, i];
        attrib[!, "C_M_" * cty_list[i]] .= model["C_M"][:, i];
        attrib[!, "C_D_sigma_init_" * cty_list[i]] .= model["C_D_sigma_init"][:, i];
        attrib[!, "C_D_sigma_" * cty_list[i]] .= model["C_D_sigma"][:, i];
        attrib[!, "C_C_sigma_init_" * cty_list[i]] .= model["C_C_sigma_init"][:, i];
        attrib[!, "C_C_sigma_" * cty_list[i]] .= model["C_C_sigma"][:, i];
        attrib[!, "C_M_sigma_init_" * cty_list[i]] .= model["C_M_sigma_init"][:, i];
        attrib[!, "C_M_sigma_" * cty_list[i]] .= model["C_M_sigma"][:, i];
        attrib[!, "S_C_init_" * cty_list[i]] .= model["S_C_init"][:, i];
        attrib[!, "S_C_" * cty_list[i]] .= model["S_C"][:, i];
        attrib[!, "S_M_init_" * cty_list[i]] .= model["S_M_init"][:, i];
        attrib[!, "S_M_" * cty_list[i]] .= model["S_M"][:, i];
        attrib[!, "S_L_init_" * cty_list[i]] .= model["S_L_init"][:, i];
        attrib[!, "S_L_" * cty_list[i]] .= model["S_L"][:, i];
        attrib[!, "cent_C_init_" * cty_list[i]] .= model["cent_C_init"][:, i];
        attrib[!, "cent_C_" * cty_list[i]] .= model["cent_C"][:, i];
        attrib[!, "cent_M_init_" * cty_list[i]] .= model["cent_M_init"][:, i];
        attrib[!, "cent_M_" * cty_list[i]] .= model["cent_M"][:, i];
        attrib[!, "adj_cent_C_init_" * cty_list[i]] .= model["adj_cent_C_init"][:, i];
        attrib[!, "adj_cent_C_" * cty_list[i]] .= model["adj_cent_C"][:, i];
        attrib[!, "adj_cent_M_init_" * cty_list[i]] .= model["adj_cent_M_init"][:, i];
        attrib[!, "adj_cent_M_" * cty_list[i]] .= model["adj_cent_M"][:, i];

        # derivatives for M
        attrib[!, "dM_CHN_logw_init_" * cty_list[i]] .= model["dM_logw_init"][i, 1];
        attrib[!, "dM_USA_logw_init_" * cty_list[i]] .= model["dM_logw_init"][i, 2];
        attrib[!, "dM_CHN_logw_" * cty_list[i]] .= model["dM_logw"][i, 1];
        attrib[!, "dM_USA_logw_" * cty_list[i]] .= model["dM_logw"][i, 2];

        attrib[!, "dM_CHN_logp_init"] .= model["dM_logp_init"][:, 1];
        attrib[!, "dM_USA_logp_init"] .= model["dM_logp_init"][:, 2];
        attrib[!, "dM_CHN_logp"] .= model["dM_logp"][:, 1];
        attrib[!, "dM_USA_logp"] .= model["dM_logp"][:, 2];

        attrib[!, "dM_CHN_logP_C_init_" * cty_list[i]] .= model["dM_logP_C_init"][i, 1];
        attrib[!, "dM_USA_logP_C_init_" * cty_list[i]] .= model["dM_logP_C_init"][i, 2];
        attrib[!, "dM_CHN_logP_C_" * cty_list[i]] .= model["dM_logP_C"][i, 1];
        attrib[!, "dM_USA_logP_C_" * cty_list[i]] .= model["dM_logP_C"][i, 2];

        attrib[!, "dM_CHN_logP_M_init_" * cty_list[i]] .= model["dM_logP_M_init"][i, 1];
        attrib[!, "dM_USA_logP_M_init_" * cty_list[i]] .= model["dM_logP_M_init"][i, 2];
        attrib[!, "dM_CHN_logP_M_" * cty_list[i]] .= model["dM_logP_M"][i, 1];
        attrib[!, "dM_USA_logP_M_" * cty_list[i]] .= model["dM_logP_M"][i, 2];

        attrib[!, "dM_CHN_X_init"] .= model["dM_X_init"][:, 1];
        attrib[!, "dM_USA_X_init"] .= model["dM_X_init"][:, 2];
        attrib[!, "dM_CHN_X"] .= model["dM_X"][:, 1];
        attrib[!, "dM_USA_X"] .= model["dM_X"][:, 2];

        attrib[!, "dM_CHN_R_init_" * cty_list[i]] .= model["dM_R_init"][i, 1];
        attrib[!, "dM_USA_R_init_" * cty_list[i]] .= model["dM_R_init"][i, 2];
        attrib[!, "dM_CHN_R_" * cty_list[i]] .= model["dM_R"][i, 1];
        attrib[!, "dM_USA_R_" * cty_list[i]] .= model["dM_R"][i, 2];
    end
    attrib[!, "c_ratio_init"] = attrib[!, "c_init_CHN"] ./ attrib[!, "c_init_USA"];
    attrib[!, "c_ratio"] = attrib[!, "c_CHN"] ./ attrib[!, "c_USA"];
    attrib[!, "delta_c_ratio"] = (attrib[!, "c_ratio"] - attrib[!, "c_ratio_init"]) ./ attrib[!, "c_ratio_init"] .* 100;
    attrib[!, "m_ratio_init"] = attrib[!, "m_init_CHN"] ./ attrib[!, "m_init_USA"];
    attrib[!, "m_ratio"] = attrib[!, "m_CHN"] ./ attrib[!, "m_USA"];
    attrib[!, "delta_m_ratio"] = (attrib[!, "m_ratio"] - attrib[!, "m_ratio_init"]) ./ attrib[!, "m_ratio_init"] .* 100;

    # taxes
    save_list = [(1, 1), (2, 1), (3, 1), (1, 2), (2, 2), (3, 2)];
    pair_list = ["CHN_CHN", "USA_CHN", "ROW_CHN", "CHN_USA", "USA_USA", "ROW_USA"];
    for j in 1:6
        # elasticities
        imp_ix = save_list[j][1];
        exp_ix = save_list[j][2];
        pair_name = pair_list[j];

        add_tax_columns("elast_init_" * pair_name, imp_ix, exp_ix, -diag(model["dT_logF_init"][imp_ix][exp_ix]));
        add_tax_columns("elast_" * pair_name, imp_ix, exp_ix, -diag(model["dT_logF"][imp_ix][exp_ix]));

        add_tax_columns("tau_X_start_" * pair_name, imp_ix, exp_ix, model["tau_X_start"][imp_ix][exp_ix]);
        add_tax_columns("tau_X_init_" * pair_name, imp_ix, exp_ix, model["tau_X_init"][imp_ix][exp_ix]);
        add_tax_columns("tau_X_" * pair_name, imp_ix, exp_ix, model["tau_X"][imp_ix][exp_ix]);
        add_tax_columns("tau_M_start_" * pair_name, imp_ix, exp_ix, model["tau_M_start"][imp_ix][exp_ix]);
        add_tax_columns("tau_M_init_" * pair_name, imp_ix, exp_ix, model["tau_M_init"][imp_ix][exp_ix]);
        add_tax_columns("tau_M_" * pair_name, imp_ix, exp_ix, model["tau_M"][imp_ix][exp_ix]);
        for suffix in ["", "M"]
            add_tax_columns("T" * suffix * "_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_" * pair_name, imp_ix, exp_ix, model["T" * suffix][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_rev_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_rev_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_rev_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_rev"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_wage_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_wage_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_wage_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_wage"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_rev_row_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_rev_row_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_rev_row_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_rev_row"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_PE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C_PE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_PE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C_PE"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_PE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M_PE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_PE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M_PE"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_GE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C_GE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_GE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C_GE"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_GE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M_GE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_GE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M_GE"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_income_init_GE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_income_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_income_GE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_income"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_C_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_C"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_M_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_M"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_PE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_PE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_PE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_PE"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_GE_init_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_GE_init"][imp_ix][exp_ix]);
            add_tax_columns("T" * suffix * "_price_GE_" * pair_name, imp_ix, exp_ix, model["T" * suffix * "_price_GE"][imp_ix][exp_ix]);
            for i = 1:3
                # derivatives for T
                add_tax_columns("dT" * suffix * "_" * pair_name * "_logw_init_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logw_init"][imp_ix][exp_ix][i, :]);
                add_tax_columns("dT" * suffix * "_" * pair_name * "_logw_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logw"][imp_ix][exp_ix][i, :]);

                add_tax_columns("dT" * suffix * "_" * pair_name * "_logP_C_init_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logP_C_init"][imp_ix][exp_ix][i, :]);
                add_tax_columns("dT" * suffix * "_" * pair_name * "_logP_C_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logP_C"][imp_ix][exp_ix][i, :]);

                add_tax_columns("dT" * suffix * "_" * pair_name * "_logP_M_init_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logP_M_init"][imp_ix][exp_ix][i, :]);
                add_tax_columns("dT" * suffix * "_" * pair_name * "_logP_M_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_logP_M"][imp_ix][exp_ix][i, :]);

                add_tax_columns("dT" * suffix * "_" * pair_name * "_R_init_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_R"][imp_ix][exp_ix][i, :]);
                add_tax_columns("dT" * suffix * "_" * pair_name * "_R_" * cty_list[i], imp_ix, exp_ix, model["dT" * suffix * "_R"][imp_ix][exp_ix][i, :]);
            end
        end
    end
    CSV.write(joinpath(STATS_PATH, "calibration", "model", "scenarios", name * ".csv"), attrib);
end


function main_iteration(max_iter = 1_000_000, tol_tax = 1e-4, tol_wage = -1, tol_iter = -1) 
    model = initialize_data();

    # jacobian and beta estimation
    get_dlogw_dP(model, true);
    if mil_mode != "no_mil"
        estimate_beta(model);
    end

    # hat algebra run to initialize the start
    if beta_mode == "mult"
        hat_algebra(model, true);
    else
        hat_algebra(model, false);
    end

    # first tax pass
    dist_tax = -Inf;
    for pair in active_pairs
        solve_taxes(pair[1], pair[2], model, true, true);
        dist_tax = maximum([dist_tax, maximum(abs.(model["delta_T"][pair[1]][pair[2]]))]);
    end
    for pair in active_import_pairs
        solve_taxes(pair[1], pair[2], model, false, true);
        dist_tax = maximum([dist_tax, maximum(abs.(model["delta_TM"][pair[1]][pair[2]]))]);
    end
    iter_wage = Inf;
    dist_wage = Inf;

    # iteration to close
    for iter in 1:max_iter
        # save results
        if dist_tax < tol_tax || dist_wage < tol_wage || iter_wage < tol_iter
            dist_wage, iter_wage = hat_algebra(model, false);
            ts = now();
            ts = Dates.format(ts, "yyyy-mm-dd-HH-MM-SS");
            name = core_name * "_" * mil_mode * "_" * wgt_mode * "_" * ctf_mode * "_" * beta_mode * "_" * ts;
            write_model(model, name);
            break;
        end

        # hat algebra iteration
        dist_wage, iter_wage = hat_algebra(model, false);
        get_dlogw_dP(model, false);
        print_log("dist_wage: $dist_wage\n");
            
        # solve for taxes
        dist_tax = -Inf;
        for pair in active_pairs
            solve_taxes(pair[1], pair[2], model, true, false);
            dist_tax = maximum([dist_tax, maximum(abs.(model["delta_T"][pair[1]][pair[2]]))]);
        end
        for pair in active_import_pairs
            solve_taxes(pair[1], pair[2], model, false, false);
            dist_tax = maximum([dist_tax, maximum(abs.(model["delta_TM"][pair[1]][pair[2]]))]);
        end
        print_log("*** TAX ITERATION $iter DISTANCE $dist_tax WAGE $dist_wage ***");
    end
end

