# ===================================================================
# The contents of this file are dedicated to the public domain.  To
# the extent that dedication to the public domain is not available,
# everyone is granted a worldwide, perpetual, royalty-free,
# non-exclusive license to exercise all rights associated with the
# contents of this file for any purpose whatsoever.
# No rights are reserved.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ===================================================================

'''
Created on Tue Oct 30 15:19:56 2018

Tool to plot vertical NEMO grids. 

@author James Harle

$Last commit on:$
'''

import numpy as np
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from matplotlib.collections import PatchCollection
from matplotlib.patches import Polygon

import seaborn as sns; sns.set()
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from mpl_toolkits.axes_grid1.inset_locator import mark_inset
from matplotlib.colors import ListedColormap
from mpl_toolkits.basemap import Basemap

def plot_zgr(f_cfg, f_bth, i_coords, j_coords, sig_lev, xlim, ylim, zoom, title):
    
    """ Plotting of the various vertical grids along with stiffness value

        Parent grid to child grid weights are defined along with rotation
        weightings for vector quantities.

        Args:
            f_cfg           (str) : domain_cfg file
            f_bth           (str) : bathy file
            i_coords        (arr) : numpy array of i coordinates
            j_coords        (arr) : numpy array of j coordinates
            sig_lev         (int) : number of sigma levels present
            xlim            (arr) : [xmin, xmax]
            ylim          (float) : deepest depth to plot
            zoom           (bool) : insert zoom of upper water column
            title           (str) : !?                     
        
        Returns: figure handle
    
        Usage:
        file_bth   = './bathymeter_mo_ps43.nc'
        file_cfg   = './domain_cfg_sf_L51_r24.nc'
        title      = 'Stiffness Factor: sf L51 r24 47$^{\circ}$N'
        fig_handle = plot_zgr(file_cfg, file_bth, np.arange(100,170,1), 
                              np.arange(100,101,1), 51, [-6.5, -2.5], 4400, 
                              True, title)
    """
    
    # need to write in a check that either i or j are single values
    
    rootgrp = Dataset(f_cfg, "r", format="NETCDF4")
    e3t_0   = np.squeeze(rootgrp.variables['e3t_0'][:,:,j_coords,i_coords])
    e3w_0   = np.squeeze(rootgrp.variables['e3w_0'][:,:,j_coords,i_coords])
    e3t_1d  = np.squeeze(rootgrp.variables['e3t_1d'][:,:])
    e3w_1d  = np.squeeze(rootgrp.variables['e3w_1d'][:,:])
    m       = np.squeeze(rootgrp.variables['bottom_level'][:,j_coords,i_coords])
    lon     = np.squeeze(rootgrp.variables['glamt'][:,j_coords,i_coords])
    lat     = np.squeeze(rootgrp.variables['gphit'][:,j_coords,i_coords])

    if sig_lev > 0:
        stiff   = np.squeeze(rootgrp.variables['stiffness'][:,j_coords,i_coords])
    else:
        stiff = np.copy(m)
        stiff[:] = 0.
        stiff = np.where(stiff<=1., 0. , stiff)
        stiff = np.where(stiff> 5., 1. , stiff)
        stiff = np.where(stiff> 1., 0.5, stiff)
    
    mv      = np.copy(m)
    mv[0:-1]= np.minimum(m[0:-1],m[1:])
    
    jpk = len(e3t_0[:,0])
    jpi = np.max(m.shape)
    
    if len(i_coords)==1:
        e3v_0   = np.squeeze(rootgrp.variables['e3v_0' ][:,:,j_coords,i_coords])
        e3vw_0  = np.squeeze(rootgrp.variables['e3vw_0'][:,:,j_coords,i_coords])
        coords  = j_coords
    elif len(j_coords)==1:
        e3v_0   = np.squeeze(rootgrp.variables['e3u_0' ][:,:,j_coords,i_coords])
        e3vw_0  = np.squeeze(rootgrp.variables['e3uw_0'][:,:,j_coords,i_coords])
        coords  = i_coords
    else:
        raise Exception('Problem with input coordinates.')
              
    gdept   , gdepw     = e3_to_depth( e3t_0,  e3w_0, jpk)
    gdepv   , gdepvw    = e3_to_depth( e3v_0, e3vw_0, jpk)
    gdept_1d, gdepw_1d  = e3_to_depth(e3t_1d[:,np.newaxis], e3w_1d[:,np.newaxis], jpk)
    
    rootgrp.close()
    
    # read in bathymetry
    
    rootgrp = Dataset(f_bth, "r", format="NETCDF4")
    bathy   = np.squeeze(rootgrp.variables['Bathymetry'][:,j_coords,i_coords])
    rootgrp.close()
        
    #if zoom==False:
    f, ax = plt.subplots(nrows=1, ncols=1, figsize=(18, 10))
    #else:
    #f, ax = plt.subplots(nrows=1, ncols=1, figsize=(9, 5))


    plt.sca(ax)
   
    bathy_patch = Polygon(np.vstack((np.hstack( (lon[0], lon, lon[-1]) ),
                                     np.hstack( (np.amax(bathy[:]), bathy, np.amax(bathy[:])) ))).T,
                          closed=True,
                          facecolor=(0.6,0.6,0.6), alpha=0.2, edgecolor=None)

    # Add patch to axes
    ax.add_patch(bathy_patch)
    patches = []
    colors = []
    
    
    for i in range(jpi-1):
        
        k = np.min((jpk-2,m[i+1]-1))
        
        if k >= sig_lev:
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            y = [gdepw[k+1,i+1], gdepw[k+1,i+1], gdepw[k+1,i+1],
                 gdepw[sig_lev,i+1], gdepw[sig_lev,i+1], gdepw[sig_lev,i+1], gdepw[k+1,i+1]]
            
            polygon = Polygon(np.vstack((x,y)).T, True)
            patches.append(polygon)
            colors = np.append(colors,stiff[i+1]) 
    
        k = np.min((sig_lev-1,m[i+1]-1))
        
        x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
             coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
        x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
        y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
             gdepvw[0,i+1], gdepw[0,i+1], gdepvw[0,i], gdepvw[k+1,i]]

        polygon = Polygon(np.vstack((x,y)).T, True)
        patches.append(polygon)
        colors = np.append(colors,stiff[i+1])
       
    # hbatt plotting
    for i in range(jpi-1):
        for k in np.arange(np.min((jpk-2,sig_lev-1)),-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
                 gdepvw[k,i+1], gdepw[k,i+1], gdepvw[k,i], gdepvw[k+1,i]]
            plt.plot(x,y,color=(0.5,0.5,0.5),linewidth=0.1)
            plt.plot(lon[i+1], gdept[k,i+1],'.',markersize=1,color=(0.5,0.5,0.5))
    
    # zps plotting up to sig_lev
    for i in range(jpi-1):
        for k in np.arange(np.min((jpk-2,m[i+1]-1)),sig_lev-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepw[k+1,i+1], gdepw[k+1,i+1], gdepw[k+1,i+1],
                 gdepw[k,i+1], gdepw[k,i+1], gdepw[k,i+1], gdepw[k+1,i+1]]
            plt.plot(x,y,'k-',linewidth=0.2)
            plt.plot(lon[i+1], gdept[k,i+1],'k.',markersize=1)
            
    # zps/sigma plotting sig_lev to surface
    for i in range(jpi-1):
        for k in np.arange(np.min((sig_lev-1,m[i+1]-1)),-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
                 gdepvw[k,i+1], gdepw[k,i+1], gdepvw[k,i], gdepvw[k+1,i]]
            plt.plot(x,y,'k-',linewidth=0.2)
            plt.plot(lon[i+1], gdept[k,i+1],'k.',markersize=1)
    plt.plot(lon,bathy,'-',color=(0.4,0,0))
    # MEs evelope
    if 'MEs' in f_cfg:
        plt.plot(lon, gdepw[23,:],'--',color=(0.1, 0.5, 0.1),markersize=1)
        
    p = PatchCollection(patches, alpha=0.6)
    p.set_array(np.array(colors))
    p.set_clim((0,10))
    cmap = sns.cubehelix_palette(n_colors=3,light=1, reverse=True, as_cmap=True)
    cmap = sns.diverging_palette(220, 20, as_cmap=True)
    cmap = ListedColormap(sns.color_palette("Spectral", 256)).reversed()
    #cmap = cmap[-1:0,:]
    p.set_cmap(cmap)
    ax.add_collection(p)
    f.colorbar(p, ax=ax, extend='max')
    ax.set_ylim((0,ylim))
    ax.set_xlim((xlim[0], xlim[1]))
    ax.invert_yaxis()
    #ax.set_title('Stiffness Factor: SZT L51 r10 47$^{o}$N', fontweight='bold')
    #if zoom==False:
    ax.set_title(title, fontweight='bold')
    ax.set_ylabel('Depth (m)')
    ax.set_xlabel('Longitude')
    
    
    axins = ax.inset_axes([3./8.-1./32., 1./9.-1./18., 5./8., 5./9.])
    #axins.imshow(Z2, extent=extent, origin="lower")
    # sub region of the original image
    x1, x2, y1, y2 = xlim[0], xlim[1], 0., 500.
    axins.set_xlim(x1, x2)
    axins.set_ylim(y1, y2)
    #axins.set_xticklabels('')
    #axins.set_yticklabels('')


   
    bathy_patch = Polygon(np.vstack((np.hstack( (lon[0], lon, lon[-1]) ),
                                     np.hstack( (np.amax(bathy[:]), bathy, np.amax(bathy[:])) ))).T,
                          closed=True,
                          facecolor=(0.6,0.6,0.6), alpha=0.2, edgecolor=None)

    # Add patch to axes
    axins.add_patch(bathy_patch)
    patches = []
    colors = []
    
    
    for i in range(jpi-1):
        
        k = np.min((jpk-2,m[i+1]-1))
        
        if k >= sig_lev:
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            y = [gdepw[k+1,i+1], gdepw[k+1,i+1], gdepw[k+1,i+1],
                 gdepw[sig_lev,i+1], gdepw[sig_lev,i+1], gdepw[sig_lev,i+1], gdepw[k+1,i+1]]
            
            polygon = Polygon(np.vstack((x,y)).T, True)
            patches.append(polygon)
            colors = np.append(colors,stiff[i+1]) 
    
        k = np.min((sig_lev-1,m[i+1]-1))
        
        x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
             coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
        x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
        y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
             gdepvw[0,i+1], gdepw[0,i+1], gdepvw[0,i], gdepvw[k+1,i]]

        polygon = Polygon(np.vstack((x,y)).T, True)
        patches.append(polygon)
        colors = np.append(colors,stiff[i+1])
       
    # hbatt plotting
    for i in range(jpi-1):
        for k in np.arange(np.min((jpk-2,sig_lev-1)),-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
                 gdepvw[k,i+1], gdepw[k,i+1], gdepvw[k,i], gdepvw[k+1,i]]
            axins.plot(x,y,color=(0.5,0.5,0.5),linewidth=0.1)
            axins.plot(lon[i+1], gdept[k,i+1],'.',markersize=1,color=(0.5,0.5,0.5))
    
    # zps plotting up to sig_lev
    for i in range(jpi-1):
        for k in np.arange(np.min((jpk-2,m[i+1]-1)),sig_lev-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepw[k+1,i+1], gdepw[k+1,i+1], gdepw[k+1,i+1],
                 gdepw[k,i+1], gdepw[k,i+1], gdepw[k,i+1], gdepw[k+1,i+1]]
            axins.plot(x,y,'k-',linewidth=0.2)
            axins.plot(lon[i+1], gdept[k,i+1],'k.',markersize=1)
            
    # zps/sigma plotting sig_lev to surface
    for i in range(jpi-1):
        for k in np.arange(np.min((sig_lev-1,m[i+1]-1)),-1,-1):
        
            phw = np.mean(lon[i+1:i+3])
            mhw = np.mean(lon[i:i+2])
            x = [coords[i+1]-0.5, coords[i+1], coords[i+1]+0.5, 
                 coords[i+1]+0.5, coords[i+1], coords[i+1]-0.5, coords[i+1]-0.5]
            x = [np.mean(lon[i:i+2]), lon[i+1], np.mean(lon[i+1:i+3]), 
                 np.mean(lon[i+1:i+3]), lon[i+1], np.mean(lon[i:i+2]), np.mean(lon[i:i+2])]
            
            y = [gdepvw[k+1,i], gdepw[k+1,i+1], gdepvw[k+1,i+1],
                 gdepvw[k,i+1], gdepw[k,i+1], gdepvw[k,i], gdepvw[k+1,i]]
            axins.plot(x,y,'k-',linewidth=0.2)
            axins.plot(lon[i+1], gdept[k,i+1],'k.',markersize=1)
    axins.plot(lon,bathy,'-',color=(0.4,0,0))
    # MEs evelope 
    if 'MEs' in f_cfg:
        axins.plot(lon, gdepw[23,:],'--',color=(0.1, 0.5, 0.1),markersize=1,linewidth=2)
    p = PatchCollection(patches, alpha=0.6)
    p.set_array(np.array(colors))
    p.set_clim((0,10))
    #cmap = sns.cubehelix_palette(n_colors=3,light=1, reverse=True, as_cmap=True)
    #cmap = sns.diverging_palette(220, 20, as_cmap=True)
    #cmap = ListedColormap(sns.color_palette("Spectral",256))
    p.set_cmap(cmap)
    axins.add_collection(p)

    axins.set_ylim((0,500))
    axins.set_visible(zoom)
    #axins.set_xlim((-6.5,-2.5))
    axins.invert_yaxis()
    
    if zoom:
        ax.indicate_inset_zoom(axins)
    
    return f

def e3_to_depth(pe3t, pe3w, jpk):
    '''
    funtion e3_to_depth
    Purpose :   compute t- & w-depths of model levels from e3t & e3w scale factors
    Method  :   The t- & w-depth are given by the summation of e3w & e3t, resp. 
    Action  :   pe3t, pe3w : scale factor of t- and w-point (m)
    '''
      
    pdepw      = np.zeros_like(pe3w)
    pdepw[0,:] = 0.
    pdept      = np.zeros_like(pe3t)
    pdept[0,:] = 0.5 * pe3w[0,:]
 
    for jk in np.arange(1,jpk,1):
        pdepw[jk,:] = pdepw[jk-1,:] + pe3t[jk-1,:]
        pdept[jk,:] = pdept[jk-1,:] + pe3w[jk  ,:]
      
    return pdept, pdepw

def plot_2d_stiff(f_cfg, title):
        
    """ Plotting of the various vertical grids along with stiffness value

        Parent grid to child grid weights are defined along with rotation
        weightings for vector quantities.

        Args:
            f_cfg           (str) : domain_cfg file
            title           (str) : !?                     
        
        Returns: figure handle
    
        Usage:
        file_cfg   = './domain_cfg_sf_L51_r24.nc'
        title      = 'Stiffness Factor: sf L51 r24'
        fig_handle = plot_2d_stiff(file_cfg, title)
    """
    
    cmap = sns.diverging_palette(220, 20, as_cmap=True)
    rootgrp = Dataset(f_cfg, "r", format="NETCDF4")
    stiff   = np.squeeze(rootgrp.variables['stiffness'][:,:,:])
    m       = np.squeeze(rootgrp.variables['bottom_level'][:,:,:])
    x   = np.squeeze(rootgrp.variables['glamt'][:,:,:])
    y   = np.squeeze(rootgrp.variables['gphit'][:,:,:])
    rootgrp.close()
    stiff[m==0]=np.nan
    f, ax = plt.subplots(nrows=1, ncols=1, figsize=(8, 8))
    cmap = ListedColormap(sns.color_palette("Spectral_r", n_colors=256))
    
    map = Basemap(llcrnrlat=40.,urcrnrlat=65.1,\
                llcrnrlon=-20.,urcrnrlon=10.1,\
                rsphere=(6378137.00,6356752.3142),\
                resolution='h',projection='merc',\
                lat_0=40.,lon_0=-5.)
    #map.drawcoastlines()
    map.drawcountries()
    map.fillcontinents(color='grey')
    map.drawmeridians(np.arange(-20.,10.1,5),labels=[0,0,0,1])
    map.drawparallels(np.arange(40.,65.1,5),labels=[1,0,0,0])
    
    x1,y1 = map(x,y)
    
    a=map.pcolormesh(x1,y1,stiff,cmap=cmap,vmin=0,vmax=10, alpha=0.4)
    
    a.set_edgecolor('face')
    plt.colorbar(extend='max')
    ax.set_title(title, fontweight='bold')
    x2,y2=map([-6.5, -2.5],[47, 47])
    map.plot(x2,y2,'-',color='k', linewidth=3)
    
    return f


