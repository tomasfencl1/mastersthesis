%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% title: Household Heterogeneity and Monetary Policy Transmission in the Czech Economy
% author: Fencl Tomas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN VAR MATLAB
% desc: pulls and transforms the VAR responses, creates scaled IRF plot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the VAR is run via BEARapp, for specification details see the log below

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOG

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RESULTS
load('results_BVAR-sr_9vars.mat')
IRFvariables = cellstr(endo);  % char matrix to cell array of strings

% variables of interest
target_vars = {
    '3MPRIBOR', ...
    'mpinfl', ...
    'empl_log', ...
    'avgwage_sa_real_log', ...
    'hpind_log', ...
    'psx_log', ...
    'ndcons_real_log'
};

% define shock variable indices
var_idx_pribor = find(strcmp(IRFvariables, '3MPRIBOR'));
shock_idx = find(strcmp(IRFvariables, '3MPRIBOR'));

% extract IRFs of shock variable
irf_pribor = struct_irf_record{var_idx_pribor, shock_idx}; % assuming [respons x shock]

% median response across draws at horizon 1 (shall equal 1)
median_impact_pribor = median(irf_pribor(:, 1));  % assuming [draws x horizons]

% scale median to -1.00 (−100 b.p. shock)
scaling = -1 / median_impact_pribor;
fprintf('scaling factor for −100 bp shock: %.4f\n\n', scaling);

% loop through target variables
for i = 1:length(target_vars)
    varname = target_vars{i};
    var_idx = find(strcmp(IRFvariables, varname));
    
    irf_var = struct_irf_record{var_idx, shock_idx};
    irf_var_norm = irf_var * scaling;

    % median responses across draws at horizons 3–5
    median_h3 = median(irf_var_norm(:, 3));
    median_h4 = median(irf_var_norm(:, 4));
    median_h5 = median(irf_var_norm(:, 5));

    % average the median responses
    avg_median_response = mean([median_h3, median_h4, median_h5]);
    fprintf('avg. median response of %-25s: %+6.4f\n', varname, avg_median_response);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IRF PLOT
n_horizons = 20;
figure;

% variables for IRF plot
plot_vars = IRFvariables;

for i = 1:length(plot_vars)
    varname = plot_vars{i};
    var_idx = find(strcmp(IRFvariables, varname));

    % extract and scale IRFs
    irf_var = struct_irf_record{var_idx, shock_idx};
    irf_var_norm = irf_var * scaling;

    % desired horizon
    irf_trimmed = irf_var_norm(:, 1:n_horizons);

    % stats across draws
    median_irf = median(irf_trimmed, 1);
    lower68 = prctile(irf_trimmed, 16, 1);
    upper68 = prctile(irf_trimmed, 84, 1);
    lower90 = prctile(irf_trimmed, 5, 1);
    upper90 = prctile(irf_trimmed, 95, 1);
    q = 1:n_horizons;

    % plot
    subplot(3, 3, i);
    hold on;
    fill([q fliplr(q)], [lower90 fliplr(upper90)], [0.8 0.85 1], 'EdgeColor', 'none');  % 90% band (lighter blue)
    fill([q fliplr(q)], [lower68 fliplr(upper68)], [0.6 0.7 1], 'EdgeColor', 'none');   % 68% band (darker blue)
    plot(q, median_irf, 'k-', 'LineWidth', 2);
    yline(0, '--k');
    title(varname, 'Interpreter', 'none');
    xlabel('quarters'); ylabel('% or p.p.');
    grid on;
end

sgtitle('Impulse responses to −100 bp monetary policy shock');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% other

%3MPRIBOR	clendr	cpind_log	hpind_log	mpinfl	cloan_real_log	psx_log	ndcons_real_log	gdp_real_log	wbill_real_log
%shock_d_FRA_1X4_12

%3MPRIBOR clendr cpind_log hpind_log mpinfl cloan_real_log psx_log ndcons_real_log gdp_real_log avgwage_sa_real_log empl_log er_log
%shock_d_FRA_1X4_12

%3MPRIBOR mpinfl empl_log avgwage_sa_real_log hpind_log psx_log ndcons_real_log gdp_real_log er_log
%shock_d_FRA_1X4_12
