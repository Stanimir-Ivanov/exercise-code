x0 = [0; 1];
%x0 = [0.5; .5];
iterationLimit = 1000;
epsilon = 1E-4;

constantAlpha = 1;
steplengthParam = [constantAlpha];

result = lineSearch(    @func_newtonproblem,...
                        @grad_newtonproblem,...
                        @hes_newtonproblem,... 	%Hessian needed for pure Newton.
                        x0,...
                        [],... 			%Initial approximated hessian not needed.
                        iterationLimit,...
                        epsilon,...
                        @p_newton,...          % Directions by Newton.
                        [],...                 % No parameters necessary (see p_newton).
                        @alpha_constant,...    % Constant steplength.
                        steplengthParam);      %See alpha_constant.
                    
display(result);

% CODE FOR PLOTTING %

% Define domain
x1 = linspace(-5, 5, 30);
x2 = linspace(-5, 5, 30);

% Convert to grid matrices
[X1, X2] = meshgrid(x1,x2);

% Define Z such that Z(i,j) = f(X(i,j),Y(i,j))
Z = zeros(size(X1));
for i = 1:size(X1,1)
    for j = 1:size(X1,2)
        Z(i,j) = func_newtonproblem([X1(i,j);X2(i,j)]);
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
    funcValues(i) = func_newtonproblem(result(:,i));
end

% Add iterations
hold on %to work in same figure
plot3(result(1,:), result(2,:), funcValues,'-vr','MarkerSize',10,'MarkerFaceColor','r');

% I create a separate contour plot, so I change for which z-value the
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

result(:,end) %display last iterate
funcValues(end) %display last function value