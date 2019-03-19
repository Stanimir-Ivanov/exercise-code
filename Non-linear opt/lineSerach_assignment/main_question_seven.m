x0             = [1; 1];
iterationLimit = 1000;
epsilon       = 1E-4;

alphamax = 1;
c        = 1E-4;
rho      = 0.9;
backtrackingParam = [alphamax, c, rho];

B0 = eye(2);

result = lineSearch(    @func_seven,...
                        @grad_seven,...
                        [],...                   % Hessian not needed for BFGS.
                        x0,...
                        B0,...                   % Initial approximation of Hessian
                        iterationLimit,...
                        epsilon,...
                        @p_newton_quasi_BFGS,... % Directions by BFGS.
                        [],...                   % No parameters necessary (see p_BFGS).
                        @alpha_backtracking,...  % Backtracking steplengths.
                        backtrackingParam);      % See alpha_backtracking.

display(result);

% CODE FOR PLOTTING %                    
                    
% Define domain
x1 = linspace(-1, 13, 31);
x2 = linspace(-1, 13, 31);

% Convert to grid matrices
[X1, X2] = meshgrid(x1,x2);

% Define Z such that Z(i,j) = f(X(i,j),Y(i,j))
Z = zeros(size(X1));
for i = 1:size(X1,1)
    for j = 1:size(X1,2)
        Z(i,j) = func_seven([X1(i,j);X2(i,j)]);
    end
end

% Create surface plot
surfc(X1,X2,Z);
xlabel('x1');
ylabel('x2');
zlabel('f(x2,x2)');

% Function values of iterates
funcValues = zeros(1,size(result,2));
for i = 1:size(result,2)
    funcValues(i) = func_seven(result(:,i));
end

% Add iterations
hold on %to work in same figure
plot3(result(1,:), result(2,:), funcValues,'-vr','MarkerSize',10,'MarkerFaceColor','r');

% Create contour plot
%[~,h] = contour(X1,X2,Z);

% Change elevation of contour plot
% elevation = 30;
% hh = get(h,'Children');
% for i=1:numel(hh)
%     set(hh(i),'ZData',elevation*ones(size(get(hh(i),'XData'))));
% end
hold off
figure(gcf)

result(:,end) %display last iterate
funcValues(end) %display last function value