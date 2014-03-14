% use % to start comment lines
%example of plotting a function
%plot a(t)=5*(1+t+exp(t)) from -2 to 2

t = [0:0.1:3];   %creates a vector of t values from -2 to 2 with .1 increments
u = [0:0.5:3];
a = -(t.^2) - 2*t +exp(t);    %computes vector of a values.  semicolon after the
b = ((t.^2) - 2) * 0.1;
b = [1.0 b];
for j = [2:1:length(b)],
b(j) = b(j) + (1.1)*b(j-1);
end
b = b(1:length(b)-1);

t2 = [0:0.5:3];
b2 = ((t2.^2) - 2) * 0.5;
b2 = [1.0 b2];

for j = [2:1:length(b2)],
b2(j) = b2(j) + (1.5)*b2(j-1);
end
b2 = b2(1:length(b2)-1);
%expression suppresses echoing of output into command window
plot(t,a,t,b,u,b2)   % plots t on x-axis, and a on y-axis.  Plot will autoscale
xlabel('t')
ylabel('a')

grid    %adds a grid
%to plot points instead of a line, use plot(t,a,'o')
%to plot point and a line, use plot(t,a,'o-')

