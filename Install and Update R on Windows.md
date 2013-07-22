## Install and Update R on Windows

Xiaojun Sun, 2014.5.14

R is a widely used statistical software around the world. But it's update process is very unfriendly. Every time when you try to update R you have to download all packages again. Fortunately, Dirk Eddelbuettel suggested another idea for updating R. His method is described in [Tal Galili's blog post][1]. This method can set up a global library for you, then the packages you download before the updating can be used in the new version of R. Here I describe the brief steps to install and update R for the R beginners. 

1. Install a old version of R, you can download from [this link][2]. You need to do this because you need to set up a global library during the updating process of R. If  you don't have a old version of R, how can you _update_ it. Don't worry. You can uninstall the old version after you updating it.
2. Download and install the new version of R.
3. Open your old R and run 
```
source("http://www.r-statistics.com/wp-content/uploads/2010/04/upgrading-R-on-windows.r.txt")
Old.R.RunMe()
#wait until it finishes
```

Once you have done this, then from now on, whenever you want to update to a new version of R in the future, all you will need to do are the following **TWO** (instead of three) steps:

1. Download and install the new version of R
2. Open your new R and run

```
source("http://www.r-statistics.com/wp-content/uploads/2010/04/upgrading-R-on-windows.r.txt")
New.R.RunMe()
#wait until it finishes
```

That's all. I hope it helps you. O(∩_∩)O

[1]: http://www.r-statistics.com/2010/04/changing-your-r-upgrading-strategy-and-the-r-code-to-do-it-on-windows/
[2]: http://pan.baidu.com/s/1mgNr2Jm

