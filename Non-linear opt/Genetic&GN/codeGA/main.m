% THIS CODE HAS NOT BEEN TESTED ON OCTAVE
% OCTAVE WOULD REQUIRE AT LEAST REMOVING rng(9999) ON LINE 13
% AND INSTALLING THE statistics PACKAGE
% WE RECOMMEND RUNNING ON MATLAB

n         =   25;                       % Number of individuals in a population.
n_gen     =   50;                       % Number of generations.
prop_comb = 0.30;                       % Proportion to combine.
prop_mut  = 0.50;                       % Proportion to mutate.
factor    = 0.1;                        % Multiplication factor for mutation
bounds    = [-3 -3;3 3];                % 2 by n_par matrix with min and max per parameter

rng(9999)                              % Initialize random generator to create 
                                       % reproducable results

pop      = rand(n, 2);                 % Create population of random values

[x_min, f_min, pop, pop_all] = GeneticAlgorithm( ...
                  @func_peaks, ....    % Name of the function    
                  [], ...              % If needed, parameters to pass on to func
                  pop, ...             % n by n_par matrix with population
                  n_gen, ...           % Number of generations
                  prop_comb, ...       % proportion of pop to be combined
                  prop_mut, ...        % proportion of pop to be mutated
                  factor, ...          % Multiplication factor for mutation
                  bounds);             % 2 by n_par matrix with min and max per parameter

[X, Y] = meshgrid(linspace(bounds(1,1), bounds(2,1), 31), ...
                  linspace(bounds(1,1), bounds(2,1), 31) );
Z    = zeros(size(X));            % Initialize Z to be of the size of X
Z(:) = func_peaks([X(:), Y(:)]);  % Compute the function values
meshc(X, Y, Z)                    % Make a 3D surface plot
xlabel('x')
ylabel('y')
zlabel('f(x,y)')
handle_title = title(['Generation 1']);

figure(gcf)                       % Make current window the active one.

hold on                           % Allow for overlay plotting
handle_pop = zeros(n,1);          % Initialize the vector of handles for the lines
v = axis;                         % Get the axes limits (we need the 5-th element: zmin)

for i=1:(n - 1)
    % create blue line for individual i
    handle_pop(i) = plot3(pop_all(i, 1, 1)*[1 1], ...   % x-coordinates
                          pop_all(i, 2, 1)*[1 1], ...   % y-coordinates
                          [v(5),func_peaks(pop_all(i, :, 1))], ... % z-coordinates
                          '-ob', 'Markerfacecolor', 'blue') ;
end
% Create red line for best found so far
handle_pop(n) =     plot3(pop_all(n, 1, 1)*[1 1], ...   % x-coordinates
                          pop_all(n, 2, 1)*[1 1], ...   % y-coordinates
                          [v(5),func_peaks(pop_all(n, :, 1))], ... % z-coordinates
                          '-or', 'Markerfacecolor', 'red') ;
pause(0.5)                % Wait 0.5 seconds and draw current figure
                      
for k=1:n_gen
    for i=1:n
        % Adapt the position of line for individual i
      set(handle_pop(i), 'XData', pop_all(i, 1, k)*[1 1], ...   % x-coordinates
                         'YData', pop_all(i, 2, k)*[1 1], ...   % y-coordinates
                         'ZData', [v(5),func_peaks(pop_all(i, :, k))]); % z-coordinates
    end
    set(handle_title, 'string', ['Generation ',int2str(k)]) % Update the title
    pause(0.5)             % Wait 0.5 seconds and draw current figure
end
                         
hold off                   % End overlay plotting

figure(gcf)