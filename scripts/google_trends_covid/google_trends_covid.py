import pandas as pd
import pytrends
from pytrends.request import TrendReq
import plotly.express as px
import plotly.graph_objects as go
import io

pytrends = TrendReq(hl='en-US', tz=360)
kw_list = ["covid test"]
pytrends.build_payload(kw_list = kw_list, timeframe='today 5-y', geo='US')

hist = pytrends.interest_over_time().reset_index()
hist = hist[hist['date'] >= '2020-01-01']
hist.tail()

fig_line = px.line(hist, x='date', y="covid test")

fig_line['data'][0]['line']['color']='rgb(94,25,20)'
fig_line['data'][0]['line']['width']=3

title = '<b>Google Search Interest for </b><i>"covid test"</i><b> Over Time</b>'

fig_line.update_layout(
    
     title={
        'text': title,
        'y':0.95,
        'x':0.5,
        'xanchor': 'center',
        'yanchor': 'top'},
    
    font_family="Arial",
    font_color="black",
    title_font_family="Arial",
    title_font_color="black",
    legend_title_font_color="black",

    #paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)'
)

fig_line.update_yaxes(title ="", linewidth=2, linecolor='black', gridcolor='rgb(220,220,220)')
fig_line.update_xaxes(title ="", linewidth=2, linecolor='black', gridcolor='rgb(220,220,220)')

by_state = pytrends.interest_by_region(resolution='COUNTRY', inc_low_vol=True, inc_geo_code=True).reset_index()
by_state['country'] = 'US'
by_state['state_code'] = by_state['geoCode'].str.replace('US-', '').str.strip()

fig = go.Figure(data=go.Choropleth(
                    locations=by_state['state_code'],
                    z=by_state['covid test'].astype(float),
                    locationmode='USA-states',
                    #colorscale='Reds',
                    colorscale=[[0, 'rgb(205,92,92)'], [1, 'rgb(94,25,20)']],
                    autocolorscale=False,
                    text=by_state['geoName'], # hover text
                    marker_line_color='white', # line markers between states
                    colorbar_title="",
                    
))

title = '<b>Google Search Interest for </b><i>"covid test"</i><b> by State</b>'

fig.update_layout(
    title={
        'text': title,
        'y':0.95,
        'x':0.5,
        'xanchor': 'center',
        'yanchor': 'top'},
    geo = dict(
        scope='usa',
        projection=go.layout.geo.Projection(type = 'albers usa'),
        showlakes=True, # lakes
        lakecolor='rgb(255, 255, 255)'),
    
    font_family="Arial",
    font_color="black",
    title_font_family="Arial",
    title_font_color="black",
    legend_title_font_color="black",
    paper_bgcolor='rgba(0,0,0,0)',
    plot_bgcolor='rgba(0,0,0,0)'
)

line_html = fig_line.to_html(full_html=False, include_plotlyjs='cdn')
map_html = fig.to_html(full_html=False, include_plotlyjs='cdn')


#with open('boilerplate.txt', 'r') as file:
with open('google_trends_covid.html', 'r+') as f:
    # read a list of lines into data
    data = f.readlines()
    print(len(data))

# if len(data) == 4000:
#     with open('google_trends_covid.html', 'a') as f:
#         f.write(fig_line.to_html(full_html=False, include_plotlyjs='cdn'))
#         f.write(fig.to_html(full_html=False, include_plotlyjs='cdn'))
#         print('writing the old way')
if len(data) >= 348:
    with open('google_trends_covid.html', 'r') as f:
        data = f.readlines()
        data[-2] = line_html
        data[-1] = map_html

    with open('google_trends_covid.html', 'w') as f:
        for line in data:
            f.write(line)
        print('writing the new way')

print(len(data))



