x0             = [4; 2];
iterationLimit = 1000;
epsilon        = 1E-4;

constantParam = 1; % not used
c1            = 1E-4; % strong Wolfe params such that 0 < c1 < c2 < 1
c2            = 0.9; %
alpha_1       = 1; % starting step length
alpha_max     = realmax; % set maximum step length to inf
gamma         = 2; 

steplengthParam = [c1, c2, alpha_1, alpha_max, gamma];

result = lineSearch(    @func_part1,...
                        @grad_part1,...
                        [],...     %exact Hessian is not needed for quasi-Newton methods.
                        x0,...
                        eye(length(x0)),...                  %Initial approximated hessian is an identity.
                        iterationLimit,...
                        epsilon,...
                        @p_newton_quasi_BFGS,...           %Directions by quasi-Newtion BFGS.
                        [],...                  %No parameters necessary .
                        @alpha_strongWolfe,...     %Strong Wolfe steplength.
                        steplengthParam);     %See steplengthParam definition.

% Define domain
x1 = linspace(min(result(1,:)) - std(result(1,:)), ...
              max(result(1,:)) + std(result(1,:)), 31);
x2 = linspace(min(result(2,:)) - std(result(2,:)), ...
              max(result(2,:)) + std(result(2,:)), 31);

% Convert to grid matrices
[X1, X2] = meshgrid(x1,x2);

% Define Z such that Z(i,j) = f(X(i,j),Y(i,j))
Z = zeros(size(X1));
for i = 1:size(X1,1)
    for j = 1:size(X1,2)
        Z(i,j) = func_part1([X1(i,j);X2(i,j)]);
    end
end

% Create surface plot
surf(X1,X2,Z);
xlabel('x1');
ylabel('x2');
zlabel('f(x2,x2)');

% Function values of iterates
funcValues = zeros(1,size(result,2));
for i = 1:size(result,2)
    funcValues(i) = func_part1(result(:,i));
end

% Add iterations
hold on %to work in same figure
plot3(result(1,:), result(2,:), funcValues,'-vr','MarkerSize',10,'MarkerFaceColor','r');

% I create a separate contour plot, so I can change for which z-value the
% contours appear. Note that I could have used surfc instead of surf
% earlier, which gives similar results.

% Create contour plot
[~,h] = contour(X1,X2,Z);

% Change elevation of contour plot
elevation = -5;
hh = get(h,'Children');
for i=1:numel(hh)
    set(hh(i),'ZData',elevation*ones(size(get(hh(i),'XData'))));
end
hold off

% End of contour plot.

% Make current figure the active window
figure(gcf)

result(:,end)
funcValues(end)